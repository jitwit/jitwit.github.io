var vs_src = `#version 300 es
in vec2 pos;
out vec2 gogo;
void main(void) {
  gl_Position = vec4(pos,0,1);
  gogo = pos;
}
`;

var frag_src = `#version 300 es
precision mediump float;
in vec2 gogo;
out vec4 clr;
void main (void) {
  clr = vec4(cos(gogo.x*acos(0.0)),0.1,0.7,1);
}
`;

var vertices = new Float32Array(
    [-1,-1,1,-1,1,1,-1,1]
);


window.onload = function () {
    var gl = document.querySelector("#canvas").getContext("webgl2");

    // init step 1 
    var vs = gl.createShader(gl.VERTEX_SHADER);
    var fs = gl.createShader(gl.FRAGMENT_SHADER);
    var program = gl.createProgram();

    gl.shaderSource(vs,vs_src);
    gl.compileShader(vs);
    gl.shaderSource(fs,frag_src);
    gl.compileShader(fs);

    gl.attachShader(program,vs);
    gl.attachShader(program,fs);
    gl.linkProgram(program);
    gl.useProgram(program);
    gl.deleteProgram(program);
    
    // init step 2
    var pos = gl.getAttribLocation(program,"pos");
    var vao = gl.createVertexArray();
    var buffer = gl.createBuffer();

    gl.bindVertexArray(vao);
    gl.bindBuffer(gl.ARRAY_BUFFER,buffer);
    gl.bufferData(gl.ARRAY_BUFFER,vertices,gl.STATIC_DRAW);
    gl.vertexAttribPointer(pos,2,gl.FLOAT,false,0,0);
    gl.enableVertexAttribArray(pos);

//    gl.bufferSubData(gl.ARRAY_BUFFER,0,vertices);
//    console.log(gl.getProgramParameter(program,gl.LINK_STATUS));

    draw();

    function draw () {
        gl.viewport(0,0,gl.canvas.width,gl.canvas.height);
        gl.clearColor(0,0,0,1);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        gl.bindVertexArray(vao);
        gl.drawArrays(gl.TRIANGLE_FAN,0,4);
    };
    
//    gl.clearColor(Math.random(),0.1,0.4,Math.random());
//    gl.clear(gl.COLOR_BUFFER_BIT);

};
