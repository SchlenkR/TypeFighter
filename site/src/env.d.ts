// Vite-Import-Muster der mitkompilierten editor-Package-Quellen.
declare module "*?worker" {
  const workerCtor: { new (): Worker };
  export default workerCtor;
}
declare module "*.scss";
