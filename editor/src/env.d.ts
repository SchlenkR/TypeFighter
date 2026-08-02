// Ambient declarations for the standalone typecheck only. Consuming apps
// compile these sources with their own config (Vite provides both patterns).
declare module "*?worker" {
  const workerCtor: { new (): Worker };
  export default workerCtor;
}
declare module "*.scss";
