use crate::host_types::*;
use crate::rpc::LispClient;

use std::sync::atomic::{AtomicU32, Ordering};
use tracing::info;

static CHILD_PID: AtomicU32 = AtomicU32::new(0);

pub fn kill_subprocess() {
    let pid = CHILD_PID.load(Ordering::SeqCst);
    if pid == 0 {
        return;
    }
    info!("killing subprocess tree (PID {})", pid);

    #[cfg(windows)]
    {
        let _ = std::process::Command::new("taskkill")
            .args(&["/PID", &pid.to_string(), "/T", "/F"])
            .status();
    }

    #[cfg(unix)]
    unsafe {
        libc::kill(-(pid as i32), libc::SIGKILL);
    }
}

pub fn start_server(start: bool) {
    let rt = tokio::runtime::Runtime::new().unwrap();

    rt.block_on(async {
        // TODO: user field in UI
        let hci_path = if cfg!(target_os = "windows") {"COM6"} else {"/dev/ttyACM0"};

        let rsp: String = if start {
            // Note: `info!` macro usually requires `log::info!` or similar if imported
            println!("spawning server");
            let mut client = LispClient::new("127.0.0.1:30000").await.unwrap();
            client.call(RemoteMethod::Command(RemoteCommand::Open { path: hci_path.to_string() } )).await.unwrap()
        } else {
            println!("killing server");
            let mut client = LispClient::new("127.0.0.1:30000").await.unwrap();
            client.call(RemoteMethod::Command(RemoteCommand::Close)).await.unwrap()
        };
        println!("Lisp response {:?}", rsp);
    });
}

/// On Windows: create a Job Object so the OS kills the child tree if we exit/crash.
/// Returns a raw HANDLE that must be kept alive for the lifetime of the app.
#[cfg(windows)]
fn create_job_for_child(child: &std::process::Child) -> *mut std::ffi::c_void {
    use std::os::windows::io::AsRawHandle;

    extern "system" {
        fn CreateJobObjectW(attrs: *mut u8, name: *const u16) -> *mut std::ffi::c_void;
        fn SetInformationJobObject(
            job: *mut std::ffi::c_void,
            class: u32,
            info: *const u8,
            len: u32,
        ) -> i32;
        fn AssignProcessToJobObject(
            job: *mut std::ffi::c_void,
            process: *mut std::ffi::c_void,
        ) -> i32;
    }

    #[repr(C)]
    #[derive(Default)]
    struct BasicLimitInfo {
        per_process_user_time_limit: i64,
        per_job_user_time_limit: i64,
        limit_flags: u32,
        minimum_working_set_size: usize,
        maximum_working_set_size: usize,
        active_process_limit: u32,
        affinity: usize,
        priority_class: u32,
        scheduling_class: u32,
    }

    #[repr(C)]
    #[derive(Default)]
    struct IoCounters {
        read_operations: u64,
        write_operations: u64,
        other_operations: u64,
        read_transfer: u64,
        write_transfer: u64,
        other_transfer: u64,
    }

    #[repr(C)]
    #[derive(Default)]
    struct ExtendedLimitInfo {
        basic: BasicLimitInfo,
        io: IoCounters,
        process_memory_limit: usize,
        job_memory_limit: usize,
        peak_process_memory_used: usize,
        peak_job_memory_used: usize,
    }

    unsafe {
        let job = CreateJobObjectW(std::ptr::null_mut(), std::ptr::null());
        assert!(!job.is_null(), "CreateJobObjectW failed");

        let mut info = ExtendedLimitInfo::default();
        info.basic.limit_flags = 0x2000; // JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE

        SetInformationJobObject(
            job,
            9, // JobObjectExtendedLimitInformation
            &info as *const _ as *const u8,
            std::mem::size_of::<ExtendedLimitInfo>() as u32,
        );

        AssignProcessToJobObject(job, child.as_raw_handle() as *mut std::ffi::c_void);

        job
    }
}

pub fn start_subprocess(start: bool, _child: Option<std::process::Child>) -> Option<std::process::Child> {
    if cfg!(target_os = "linux")
    {
        return None;
    }

    if start {
        let cwd = std::env::current_dir().expect("Failed to get current directory");
        let work_dir = cwd.join("../..").canonicalize().expect("Failed to resolve work directory");

        let core_path = work_dir.join("app.core");
        let lisp_files = ["host.lisp", "ui/utils.lisp", "ui/jsonrpc.lisp"];

        // Helper to apply required environment variables to any SBCL command
        let apply_sbcl_env = |cmd: &mut std::process::Command| {
            let path = std::env::var("PATH").unwrap_or_default();

            #[cfg(windows)]
            {
                let new_path = format!("{};C:/msys64/ucrt64/bin", path);
                cmd.env("PATH", &new_path);
            }

            #[cfg(unix)]
            {
                // Note: /c/msys64/ucrt64/bin implies a Windows Unix-compatibility layer (like MSYS2 or WSL).
                // If you intend to run this on native Linux/macOS later, you'll need standard Unix paths here.
                let new_path = format!("{}:/c/msys64/ucrt64/bin", path);
                cmd.env("PATH", &new_path);
            }

            // If you are bundling SBCL, you might also need to set SBCL_HOME here:
            // cmd.env("SBCL_HOME", "/path/to/sbcl/lib/sbcl");
        };

        let rebuild_needed = || {
            let core_meta = match std::fs::metadata(&core_path) {
                Ok(m) => m,
                Err(_) => return true,
            };

            let core_time = core_meta.modified().unwrap();

            for file in &lisp_files {
                let file_path = work_dir.join(file);
                if let Ok(meta) = std::fs::metadata(&file_path) {
                    if let Ok(mtime) = meta.modified() {
                        if mtime > core_time {
                            return true;
                        }
                    }
                }
            }
            false
        };

        if rebuild_needed() {
            println!("Building SBCL core image...");

            let mut build_cmd = std::process::Command::new("sbcl");

            // 1. APPLY ENV SETUP TO THE BUILDER
            apply_sbcl_env(&mut build_cmd);

            let status = build_cmd
                .args(&[
                    "--noinform",
                    "--load", "host.lisp",
                    "--load", "ui/utils.lisp",
                    "--load", "ui/jsonrpc.lisp",
                    "--eval", "(sb-ext:save-lisp-and-die \"app.core\")"
                ])
                .current_dir(&work_dir)
                .status()
                .expect("Failed to run SBCL core builder");

            if !status.success() {
                eprintln!("Failed to compile SBCL core image.");
                return None;
            }
        }

        // Launch SBCL instantly using the prebuilt core
        let mut cmd = std::process::Command::new("sbcl");
        cmd.args(&["--noinform", "--core", "app.core", "--eval", "(progn (host:start-jsonrpc-server)(lambda () (loop do (sleep 1)))) "])
           .current_dir(&work_dir);

        // 2. APPLY ENV SETUP TO THE RUNNER
        apply_sbcl_env(&mut cmd);

        #[cfg(unix)]
        {
            use std::os::unix::process::CommandExt;
            unsafe {
                cmd.pre_exec(|| {
                    libc::prctl(libc::PR_SET_PDEATHSIG, libc::SIGKILL);
                    libc::setpgid(0, 0);
                    Ok(())
                });
            }
        }

        let proc = cmd.spawn().expect("Failed to start sbcl subprocess");
        CHILD_PID.store(proc.id(), Ordering::SeqCst);

        #[cfg(windows)]
        {
            let job = create_job_for_child(&proc);
            Box::leak(Box::new(job));
        }

        Some(proc)
    } else {
        std::thread::sleep(std::time::Duration::from_secs(2));

        kill_subprocess();
        CHILD_PID.store(0, Ordering::SeqCst);

        None
    }
}

pub fn setup_cleanup_hooks() {
    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        kill_subprocess();
        default_hook(info);
    }));

    ctrlc::set_handler(|| {
        kill_subprocess();
        std::process::exit(1);
    }).expect("Failed to set Ctrl+C handler");
}
