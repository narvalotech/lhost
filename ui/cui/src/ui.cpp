#include "imgui.h"

void render_cui(ImGuiIO& io)
{
	static float f = 0.0f;
	static int counter = 0;

	ImGui::Begin("Hello, world!");

	ImGui::Text("This is some useful text.");

	if (ImGui::Button("Button")) {
		counter++;
	}

	ImGui::SameLine();
	ImGui::Text("counter = %d", counter);

	ImGui::Text("Application average %.3f ms/frame (%.1f FPS)", 1000.0f / io.Framerate,
		    io.Framerate);
	ImGui::End();
}
