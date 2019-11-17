var vertexShaderSource = `
#version 300 es

in vec4 a_position;
uniform mat4 u_matrix;

void main() {
   gl_Position = u_matrix * a_position;
}
`;

var fragmentShaderSource = `
#version 300 es

void main () {
}
`;

window.onload = function () {
    var canvas = document.querySelector("#canvas");
    var gl = canvas.getContext("webgl2");
    gl.clearColor(Math.random(),0.1,0.4,Math.random());
    gl.clear(gl.COLOR_BUFFER_BIT);
};
