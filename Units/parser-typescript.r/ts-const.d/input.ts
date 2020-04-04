// Taken from issue #2488 submitted by @sparkcanon
import * as three from "three";

let scene: three.Scene,
  camera: three.PerspectiveCamera,
  renderer: three.WebGLRenderer;
const cubeArr = new three.Group();

// TODO: Add lighting
const setupScene = (): void => {
  scene = new three.Scene();
  camera = new three.PerspectiveCamera(
    75,
    window.innerWidth / window.innerHeight,
    0.1,
    1000
  );

  renderer = new three.WebGLRenderer();
  renderer.setSize(window.innerWidth, window.innerHeight);
  document.body.appendChild(renderer.domElement);
  camera.position.z = 20;
};

// TODO: Position the cubes in the center of the screen
const createCubeArr = (): void => {
  let cubes;

  for (let i = 0; i < 5; i++) {
    for (let r = 1; r < 5; r++) {
      cubes = createCube();
      cubes.position.set(i * 3, r * 2, 0);
      cubeArr.add(cubes);
    }
  }
  cubeArr.position.set(-5, -5, 0);
  scene.add(cubeArr);
};

// TODO: Receive shadow
const createWall = (): void => {
  const geometry = new three.PlaneGeometry(20, 20, 20);
  const material = new three.MeshBasicMaterial({
    color: 0x0000ff,
    side: three.DoubleSide,
  });
  const mesh = new three.Mesh(geometry, material);
  scene.add(mesh);
};

const createCube = (): three.Mesh => {
  const geometry, material;
  geometry = new three.BoxGeometry(1, 1, 1);
  material = new three.MeshBasicMaterial({
    color: 0x00ff00,
    side: three.DoubleSide,
  });
  const cube = new three.Mesh(geometry, material);
  return cube;
};

const animate = (): void => {
  requestAnimationFrame(animate);
  renderer.render(scene, camera);
};

setupScene();
createCubeArr();
createWall();
animate();
