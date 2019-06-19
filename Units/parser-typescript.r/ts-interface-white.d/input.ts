interface Mover {
  move(): void
  getStatus(): { speed: number }
}

interface Shaker {
  shake(): void
  getStatus(): { frequency: number }
}

interface MoverShaker extends Mover, Shaker {
  getStatus(/*)*/): { speed: number; frequency: number }
  //somethingCommentedOut(): string
  getSomething(): /* } */ void
  getSomethingSophisticated()/*
    comment
    block
    */: void

  getTpl<T>(): Promise<T>
}

interface SimpleRecord {
  propertyA: number
  propertyB: string
  readonly propertyC: []
  withoutType?
}

interface Document {
  createElement(tagName: "div"): HTMLDivElement
  createElement(tagName: "span"): HTMLSpanElement
  createElement(tagName: "canvas"): HTMLCanvasElement
  createElement(tagName: string): HTMLElement
}

interface CompilerOptions {
  strict?: boolean
  sourcePath?: string
  targetPath?: string
}

interface List<T> {
  data: T
  next: List<T>
  owner: List<List<T>>
}

interface JQuery {
  text(content: string)
}

interface JQueryStatic {
  get(url: string, callback: (data: string) => any)
  (query: string): JQuery
}

interface Array<T> {
  length: number
  [x: number]: T
  // Other members
}
