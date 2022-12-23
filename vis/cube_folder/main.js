window.onload = function() {
  const canvas = document.getElementById("canvas");
  const gl = canvas.getContext("webgl2");
  if (!gl) {
    alert("Your browser does not support webgl2");
    return;
  }
  const input = document.getElementById("input");
  const angle = document.getElementById("angle");
  input.value = 
`        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.`;

  let rafCb;
  function loop(dt) {
    if (dt && rafCb) {
      rafCb(dt);
    }
    requestAnimationFrame(loop);
  }
  loop(null);

  let resizeCb;
  function setSize() {
    canvas.width = canvas.clientWidth;
    canvas.height = canvas.clientHeight;
    if (resizeCb) resizeCb();
    if (rafCb) rafCb();
  }
  setSize();
  window.onresize = setSize;

  function compileShader(type, src) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, src);
    gl.compileShader(shader);
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
      const info = gl.getShaderInfoLog(shader);
      throw new Error(`Could not compile WebGL shader. \n\n${info}`);
    }
    return shader;
  }
  const vertexShader = compileShader(gl.VERTEX_SHADER, `#version 300 es

precision mediump float;

in vec2 pos;

out vec2 uv;

uniform mat4 mvp;

void main() {
  uv = pos;
  gl_Position = mvp * vec4(pos, 0, 1);
}
`);
  const fragmentShader = compileShader(gl.FRAGMENT_SHADER, `#version 300 es

precision mediump float;

in vec2 uv;

out vec4 colour;

uniform sampler2D tex;

void main() {
  colour = texture(tex, uv);
}
`);
  const program = gl.createProgram();
  gl.attachShader(program, vertexShader);
  gl.attachShader(program, fragmentShader);
  gl.linkProgram(program);
  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
    const info = gl.getProgramInfoLog(program);
    throw new Error(`Could not compile WebGL program. \n\n${info}`);
  }
  gl.useProgram(program);
  const uniformMvp = gl.getUniformLocation(program, "mvp");
  const uniformTexture = gl.getUniformLocation(program, "tex");
  const inputPos = gl.getAttribLocation(program, "pos");

  const vertexData = new Float32Array([
    0, 0,
    1, 0,
    0, 1,
    1, 1,
  ]);
  const VERTEX_COUNT = 4;
  const verticesAbo = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, verticesAbo);
  gl.bufferData(gl.ARRAY_BUFFER, vertexData, gl.STATIC_DRAW);
  gl.enableVertexAttribArray(inputPos);
  gl.vertexAttribPointer(inputPos, 2, gl.FLOAT, false, 0, 0);

  let foldAngle = angle.valueAsNumber;

  const id = () => new DOMMatrix();
  const left = () => new DOMMatrix()
        .rotateAxisAngleSelf(0, 1, 0, foldAngle)
        .translateSelf(-1, 0, 0);
  const right = () => new DOMMatrix()
        .translateSelf(1, 0, 0)
        .rotateAxisAngleSelf(0, 1, 0, -foldAngle);
  const up = () => new DOMMatrix()
        .rotateAxisAngleSelf(1, 0, 0, -foldAngle)
        .translateSelf(0, -1, 0);
  const down = () => new DOMMatrix()
        .translateSelf(0, 1, 0)
        .rotateAxisAngleSelf(1, 0, 0, foldAngle);

  const textures = new Array(6).fill(0).map(_ => gl.createTexture());
  for (let i = 0; i < textures.length; ++i) {
    gl.activeTexture(gl.TEXTURE0 + i);
    gl.bindTexture(gl.TEXTURE_2D, textures[i]);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
  }

  function parseNet() {
    let lines = input.value.split("\n");
    const emptyIdx = lines.indexOf("");
    lines = emptyIdx === -1 ? lines : lines.slice(0, emptyIdx);
    const width = Math.max(...lines.map(it => it.length));
    const height = lines.length;
    // width:height are in the ratio 4:3 or 3:4
    const tileSize = Math.max(width, height) - Math.min(height, width);
    const isEmpty = (tile) => tile === undefined || tile === ' ';
    const hasTile = ([u, v]) => { const line = lines[tileSize * v];
                                  return line && !isEmpty(line[tileSize * u]); };
    const root = (() => {
      for (let u = 0; u < width; ++u) {
        for (let v = 0; v < height; ++v) {
          const pos = [u, v];
          if (hasTile(pos)) return pos;
        }
      }
    })();

    let counter = 0;
    const tileColours = {
      ".": new Uint8Array([0xff, 0xff, 0xff, 0xff]),
      "#": new Uint8Array([0x0a, 0x0a, 0x0a, 0xff]),
    };
    const colours = [
      new Float32Array([0.9, 0, 0, 1]),
      new Float32Array([0.9, 0.5, 0, 1]),
      new Float32Array([0.8, 0.8, 0, 1]),
      new Float32Array([0, 0.8, 0, 1]),
      new Float32Array([0, 0, 0.8, 1]),
      new Float32Array([0.6, 0, 0.8, 1]),
    ];
    const seen = new Set();
    seen.add(String(root));
    function buildTree(trans, from) {
      const [u, v] = from;
      const children = [[right, u+1, v],
                        [left, u-1, v],
                        [up, u, v-1],
                        [down, u, v+1]]
            .filter(([_, u1, v1]) => {
              const pos = [u1, v1];
              if (!hasTile(pos)) return false;
              const key = String(pos);
              if (seen.has(key)) return false;
              seen.add(key);
              return true;
            })
            .map(([dir, u1, v1]) => buildTree(dir, [u1, v1]));
      const texture = new Uint8Array(4 * tileSize * tileSize);
      let i = 0;
      for (let y = 0; y < tileSize; ++y) {
        for (let x = 0; x < tileSize; ++x) {
          const x1 = tileSize * u + x;
          const y1 = tileSize * v + y;
          const tile = lines[y1][x1];
          const col = tileColours[tile];
          for (let j = 0; j < 4; ++j) {
            texture[i++] = col[j];
          }
        }
      }
      gl.bindTexture(gl.TEXTURE_2D, textures[counter]);
      gl.texImage2D(
        gl.TEXTURE_2D,
        0, // level
        gl.RGBA,
        tileSize,
        tileSize,
        0, // border
        gl.RGBA,
        gl.UNSIGNED_BYTE,
        texture
      );
      return { trans, children, texture: counter++ };
    }
    return buildTree(id, root);
  }
  let tree = parseNet();
  input.onchange = () => {
    tree = parseNet();
  };

  // bottom, top, left, right, near, far
  function ortho(b, t, l, r, n, f) {
    return new DOMMatrix([
      2/(r-l), 0, 0, -(r+l)/(r-l),
      0, 2/(t-b), 0, -(t+b)/(t-b),
      0, 0, -2/(f-n), -(f+n)/(f-n),
      0, 0, 0, 1
    ]);
  }

  let yaw = 45;
  let pitch = 60;
  let scale = 1;

  function onDrag(dx, dy) {
    dy = 4*dy / canvas.height;
    dx = 4*dx / canvas.width;
    pitch += dy / Math.PI * 180;
    yaw += dx / Math.PI * 180;
  }
  canvas.onmousemove = function(evt) {
    if ((evt.buttons & 0x1) !== 0) {
      onDrag(evt.movementX, evt.movementY);
    }
  }
  canvas.onmousewheel = function(evt) {
    scale *= Math.pow(1.01, evt.deltaY);
  }

  let touches = [];
  canvas.addEventListener("touchstart", function(evt) {
    for (const t of evt.touches) {
      touches[t.identifier] = t;
    }
  });
  canvas.addEventListener("touchend", function(evt) {
    touches = [];
  });
  canvas.addEventListener("touchmove", function(evt) {
    switch (evt.touches.length) {
    case 1: {
      let touch = evt.touches[0];
      let lastTouch = touches[touch.identifier];
      if (lastTouch) {
        onDrag(
          touch.clientX - lastTouch.clientX,
          touch.clientY - lastTouch.clientY,
        );
      }
      touches[touch.identifier] = touch;
      break;
    }
    case 2: {
      let t1 = evt.touches[0];
      let t2 = evt.touches[1];
      let lt1 = touches[t1.identifier];
      let lt2 = touches[t2.identifier];
      if (lt1 && lt2) {
        const len1 = Math.hypot(
          lt1.clientX - lt2.clientX,
          lt1.clientY - lt2.clientY
        );
        const len2 = Math.hypot(
          t1.clientX - t2.clientX,
          t1.clientY - t2.clientY
        );
        scale *= len2 / len1;
      }
      touches[t1.identifier] = t1;
      touches[t2.identifier] = t2;
      break;
    }
    }
  });


  function drawTree() {
    foldAngle = angle.valueAsNumber;
    const aspect = canvas.width / canvas.height;
    const view = new DOMMatrix()
          .translateSelf(-0.5, -0.5, -0.5);
    const proj = new DOMMatrix()
          .rotateAxisAngleSelf(1, 0, 0, -90)
          .rotateAxisAngleSelf(0, 1, 0, -yaw)
          .rotateAxisAngleSelf(1, 0, 0, -pitch)
          .translateSelf(0, 0, 1)
          .invertSelf()
          .preMultiplySelf(ortho(-1, 1, -aspect, aspect, 0.1, 100))
          .scaleSelf(scale, scale, scale);
    function walkTree(trans, node) {
      gl.uniformMatrix4fv(uniformMvp, false, proj.multiply(trans).toFloat32Array());
      gl.uniform1i(uniformTexture, node.texture);
      gl.drawArrays(gl.TRIANGLE_STRIP, 0, VERTEX_COUNT);
      for (const child of node.children) {
        walkTree(trans.multiply(child.trans()), child);
      }
    }
    walkTree(view.multiply(tree.trans()), tree);
  }

  gl.enable(gl.DEPTH_TEST);
  rafCb = () => {
    gl.viewport(0, 0, canvas.width, canvas.height);
    gl.clearColor(0.8, 0.9, 1.0, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT);
    drawTree();
  }
}
