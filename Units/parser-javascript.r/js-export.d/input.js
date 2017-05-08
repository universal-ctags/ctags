/* https://developer.mozilla.org/en/docs/web/javascript/reference/statements/export#Using_named_exports */

export function cube(x) {
  return x * x * x;
}
const foo = Math.PI + Math.SQRT2;
export { cube, foo };

/* augmented from the reference */
export const pie = Math.PI;
