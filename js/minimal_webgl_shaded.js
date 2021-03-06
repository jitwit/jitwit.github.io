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
float pi = acos(0.0)*2.0;
void main (void) { 
  float x,y;
  x = 0.5*gogo.x+0.5;
  y = 0.5*gogo.y+0.5;
  x = max(0.0,min(1.0,x));
  y = max(0.0,min(1.0,y));
  clr = vec4(tan(13.0*x)*cos(12.0*y),pow(y,0.9),pow(x,0.8),1);
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

    gl.bindBuffer(gl.ARRAY_BUFFER,buffer);
    gl.bufferData(gl.ARRAY_BUFFER,vertices,gl.STATIC_DRAW);
    gl.vertexAttribPointer(pos,2,gl.FLOAT,false,0,0);
    gl.enableVertexAttribArray(pos);

//    gl.viewport(0,0,gl.canvas.width,gl.canvas.height);
//    gl.bindVertexArray(vao);
    gl.drawArrays(gl.TRIANGLE_FAN,0,4);
    
};
