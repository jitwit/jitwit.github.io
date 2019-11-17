window.onload = function () {
    var canvas = document.querySelector("#canvas");
    var gl = canvas.getContext("webgl2");
    gl.clearColor(Math.random(),0.3,0.4,Math.random());
    gl.clear(gl.COLOR_BUFFER_BIT);
};
