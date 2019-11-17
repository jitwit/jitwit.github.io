window.onload = function () {
    var canvas = document.querySelector("#canvas");
    var gl = canvas.getContext("webgl2");
    gl.clearColor(0.3,0.1,0.4,1);
    gl.clear(gl.COLOR_BUFFER_BIT);
};
