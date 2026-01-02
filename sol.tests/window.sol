let cam = gfx.camera(90, 0.1, 100);
let win = panic(gfx.window("Citrus Window", 640, 480, cam));
let post = panic(gfx.shader("sol.tests/shaders/default"));

while gfx.loop(win) {
    panic(gfx.draw(win, post));
}