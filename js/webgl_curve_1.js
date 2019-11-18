var vs_src = `#version 300 es
in vec2 pos;
out vec2 p;
void main(void) {
  gl_Position = vec4(pos,0,1);
  p = pos;
}
`;

var bush_thing_src = `#version 300 es
precision highp float;
in vec2 p;
out vec4 clr;
void main (void) { 
  vec3 col = vec3(0.4,0.4,0.7);
  float f = -p.y;
  col *= smoothstep (f,0.8,length(vec2(p.y,p.x*3.0*cos(p.y))));
  clr = vec4(col,1);
}
`;

var frag_src = `#version 300 es
precision highp float;
in vec2 p;
out vec4 clr;

// a sphere 
float map (in vec3 pos)
{ 
  float d = length(pos)-0.3;
  float d2 = pos.y + 0.3;
  return d; // min(d,d2);
}

vec3 calcNormal (in vec3 pos) // gradient
{
  vec2 e = vec2(0.00001,0.0);
  return normalize(vec3(map(pos+e.xyy)-map(pos-e.xyy),
                        map(pos+e.yxy)-map(pos-e.yxy),
                        map(pos+e.yyx)-map(pos-e.yyx)));
} 

void main (void) { 
  vec3 col = vec3(0.0);
  float tol = 0.000001;

  // ray marching
  // camera (ray origin)
  vec3 ro = vec3(0.0,0.0,1.0);
  // point (ray detection)
  vec3 rd = normalize(vec3(p,-1.5));

  float t = 0.0; 

  for (int i=0; i<100; i++) 
  {
    vec3 pos = ro + t*rd;
    float h = map(pos);

    if (h<tol) break;
    t += h;
    if (t>19.3) break;
  }
/// stopped at approx 45:00
  if (t<20.0)
  { 
    vec3 pos = ro + t*rd;
    vec3 nor = calcNormal(pos);
    vec3 bal_clr = vec3(1.0,0.7,0.5);
    vec3 sky_clr = vec3(0.0,0.0,0.3);
    vec3 sun_dir = normalize(vec3(0.9,0.3,0.3));
    float sun_dif = clamp(dot(nor,sun_dir),0.0,1.0);
    float sky_dif = clamp(0.32+0.5*dot(nor,vec3(0.0,1.0,0.0)),0.0,1.0);
    col = bal_clr*sun_dif;
    col += sky_clr * sky_dif;
    col *= 2.0 * nor.xyz;
  }
  
  clr = vec4(col,1);
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

    gl.drawArrays(gl.TRIANGLE_FAN,0,4);
};
