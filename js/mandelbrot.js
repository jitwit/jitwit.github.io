var vs_src = `#version 300 es
in vec2 pos;
out vec2 p;
void main(void) {
  gl_Position = vec4(pos,0,1);
  p = pos;
}
`;

var frag_src = `#version 300 es
precision lowp float;
in vec2 p;
out vec4 clr;

float iter (in vec2 c)
{
  float n = 0.0; float b = 256.0; vec2 z = vec2(0.0);

  for (;n<200.0;n++) 
  { z = vec2(z.x*z.x-z.y*z.y,2.0*z.x*z.y) + c; // z <- z^2 + c
    if (dot(z,z)>b*b) break;
  }

  return (n - log2(log2(dot(z,z))) + 4.0)/14.0;
}

void main (void) { 
  float s = iter(1.5*(p-vec2(0.5,0.0)));
  vec3 c = vec3(s,0.3+0.4*s,(1.0-s)*0.39);
  clr = vec4(c,1.0);
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
    
    // init step 2
    var pos = gl.getAttribLocation(program,"pos");
    var vao = gl.createVertexArray();
    var buffer = gl.createBuffer();

    gl.bindBuffer(gl.ARRAY_BUFFER,buffer);
    gl.bufferData(gl.ARRAY_BUFFER,vertices,gl.STATIC_DRAW);
    gl.vertexAttribPointer(pos,2,gl.FLOAT,false,0,0);
    gl.enableVertexAttribArray(pos);

    gl.drawArrays(gl.TRIANGLE_FAN,0,4);
};
