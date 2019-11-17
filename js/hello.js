
var shader_src = `
attribute vec4 position;
gl_FragColor = vec4(1,1,1,1);
void main () {
  gl_Position = position;
}
`;

var vertex_src = `
void main () {
  gl_FragColor = vec4(1,1,1,1);
};`;

function main () {
    // init window etc
    var canvas = document.querySelector("#canvas");
    var gl = canvas.getContext("webgl");
    gl.clearColor(0,0,0,1);
    gl.clear(gl.COLOR_BUFFER_BIT);

    // get the frag shader into vars
    var fshader = gl.createShader(gl.FRAGMENT_SHADER);
    gl.shaderSource(fshader,shader_src);
    gl.compileShader(fshader);
    var vshader = gl.createShader(gl.VERTEX_SHADER);
    gl.shaderSource(vshader,vertex_src);
    gl.compileShader(vshader);
    
    // now for apparently more boiler plate
    var program = gl.createProgram();
    gl.attachShader(program,vshader);
    gl.attachShader(program,fshader);
    gl.linkProgram(program);
    gl.useProgram(program);
}

window.onload = main;
