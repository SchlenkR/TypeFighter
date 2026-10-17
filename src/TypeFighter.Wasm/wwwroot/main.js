import { dotnet } from './_framework/dotnet.js';

const el = (id) => document.getElementById(id);
const show = (v) => { el('out').textContent = typeof v === 'string' ? v : JSON.stringify(v, null, 2); };

const { getAssemblyExports, getConfig } = await dotnet
    .withDiagnosticTracing(false)
    .create();

const exports = await getAssemblyExports(getConfig().mainAssemblyName);
const interop = exports.PureState.TypeFighter.Wasm.TypeFighterInterop;
window.tf = interop;

el('status').textContent = 'Runtime loaded, running Init (reference pool + warm-up compile) ...';
const t0 = performance.now();
const init = JSON.parse(interop.Init("[]"));
const initMs = performance.now() - t0;
if (init.error) {
    el('status').textContent = `Init FAILED: ${init.error}`;
    el('status').dataset.ready = 'failed';
    show(init);
} else {
    el('status').textContent = `Ready - TypeFighter ${init.version}, ${init.referenceCount} refs, warmup ${init.warmupMs.toFixed(0)} ms (total init ${initMs.toFixed(0)} ms)`;
    el('status').dataset.ready = '1';
    show(init);
    for (const id of ['compileinfo', 'eval', 'prelude', 'completion']) el(id).disabled = false;
}

const timed = (fn) => {
    const t = performance.now();
    const r = JSON.parse(fn());
    el('status').textContent = `${(performance.now() - t).toFixed(0)} ms`;
    show(r);
};

el('compileinfo').addEventListener('click', () => timed(() => interop.CompileInfo(el('code').value)));
el('eval').addEventListener('click', () => timed(() => interop.Eval(el('code').value)));
el('prelude').addEventListener('click', () => timed(() => interop.Prelude()));
el('completion').addEventListener('click', () => timed(() => interop.CompletionAt(el('code').value, el('code').value.length)));
