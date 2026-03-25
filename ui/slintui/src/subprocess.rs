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

pub fn start_server(start: bool, _child: Option<std::process::Child>) -> Option<std::process::Child> {
    let rt = tokio::runtime::Runtime::new().unwrap();

    if start {
        let cwd = std::env::current_dir().expect("Failed to get current directory");
        let work_dir = cwd.join("../..").canonicalize().expect("Failed to resolve work directory");

        let mut cmd = std::process::Command::new("sbcl");
        cmd.args(&["--load", "host.lisp", "--load", "ui/utils.lisp", "--load", "ui/jsonrpc.lisp", "--eval", "(loop do (sleep 1))"])
           .current_dir(&work_dir);

        #[cfg(windows)]
        {
            let path = std::env::var("PATH").unwrap_or_default();
            let new_path = format!("{};C:/msys64/ucrt64/bin", path);
            cmd.env("PATH", &new_path);
        }

        #[cfg(unix)]
        {
            let path = std::env::var("PATH").unwrap_or_default();
            let new_path = format!("{}:/c/msys64/ucrt64/bin", path);
            cmd.env("PATH", &new_path);

            use std::os::unix::process::CommandExt;
            unsafe {
                cmd.pre_exec(|| {
                    // Die when parent dies
                    libc::prctl(libc::PR_SET_PDEATHSIG, libc::SIGKILL);
                    // Own process group so we can kill the whole tree
                    libc::setpgid(0, 0);
                    Ok(())
                });
            }
        }

        let proc = cmd.spawn().expect("Failed to start sbcl subprocess");
        CHILD_PID.store(proc.id(), Ordering::SeqCst);

        #[cfg(windows)]
        {
            // Job object ensures child tree dies if we crash/get killed
            let job = create_job_for_child(&proc);
            // Box + leak keeps the handle alive without the forgetting_copy_types warning
            Box::leak(Box::new(job));
        }

        std::thread::sleep(std::time::Duration::from_secs(2));

        rt.block_on(async {
            info!("spawning server");
            let mut client = LispClient::new("127.0.0.1:55000").await.unwrap();
            let rsp: String = client.call(RemoteMethod::Command(RemoteCommand::Open)).await.unwrap();
            info!("Lisp response {:?}", rsp);
        });

        Some(proc)
    } else {
        rt.block_on(async {
            info!("killing server");
            let mut client = LispClient::new("127.0.0.1:55000").await.unwrap();
            let rsp: String = client.call(RemoteMethod::Command(RemoteCommand::Close)).await.unwrap();
            info!("Lisp response {:?}", rsp);
        });

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
