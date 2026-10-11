// Compile a WGSL module and THROW on a compile error. WebGPU reports compile errors rather than
// throwing them, so an unchecked module fails later as a blank canvas — indistinguishable from "the
// data is empty". Every renderer builds its modules through this.

export async function compileShader(
  device: GPUDevice, code: string, label: string,
  Err: new (message: string) => Error = Error,
): Promise<GPUShaderModule> {
  const module = device.createShaderModule({ code })
  const errs = (await module.getCompilationInfo()).messages.filter(m => m.type === 'error')
  if (errs.length) throw new Err(`${label}: ` + errs.map(m => `${m.lineNum}:${m.message}`).join(' | '))
  return module
}
