// Derrived from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/export
// Exporting declarations
export let name1, name2/*, … */; // also var
export const name3 = 1, name4 = 2/*, … */; // also var, let
export function functionName1() { /* … */ }
export class ClassName1 { /* … */ }
export function* generatorFunctionName1() { /* … */ }
export const { name5, name6: bar } = o;
export const [ name7, name8 ] = array;

// Export list
export { name9, /* …, */ nameA };
export { variable1 as nameB, variable2 as nameC, /* …, */ nameD };
export { variable3 as "string name" };
export { nameE as default /*, … */ };

// Default exports
export default expression;
export default function functionName2() { /* … */ }
export default class ClassName2 { /* … */ }
export default function* generatorFunctionName2() { /* … */ }
export default function () { /* … */ }
export default class { /* … */ }
export default function* () { /* … */ }

// Aggregating modules
export * from "module-name1";
export * as nameF from "module-name2";
export { nameG, /* …, */ nameH } from "module-name3";
export { import1 as nameI, import2 as nameJ, /* …, */ nameK } from "module-name4";
export { default, /* …, */ } from "module-name5";
export { default as nameL } from "module-name6";
