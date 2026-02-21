#include "imgui.h"

enum class UiEvent {
	StartScan,
	StopScan
};

static void queue_event(UiEvent event) {

}

void render_cui(ImGuiIO& io)
{
	/* TODO: start advertising button */
	static float f = 0.0f;
	static int counter = 0;

	ImGui::Begin("Hello, world!");

	ImGui::Text("This is some useful text.");

	if (ImGui::Button("Button")) {
		counter++;
	}

	ImGui::SameLine();
	if (ImGui::Button("Start Scan")) {
		queue_event(UiEvent::StartScan);
	}

	ImGui::SameLine();
	if (ImGui::Button("Stop Scan")) {
		queue_event(UiEvent::StopScan);
	}

	ImGui::SameLine();
	ImGui::Text("counter = %d", counter);

	ImGui::Text("Application average %.3f ms/frame (%.1f FPS)", 1000.0f / io.Framerate,
		    io.Framerate);
	ImGui::End();
}
