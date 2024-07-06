export function areEqual(x: any, y: any): boolean {
  if (x === y) {
    return true;
  }

  if (typeof x !== 'object' || x === null || typeof y !== 'object' || y === null) {
    return false;
  }

  const keysX = Object.keys(x);
  const keysY = Object.keys(y);

  if (keysX.length !== keysY.length) {
    return false;
  }

  for (const key of keysX) {
    if (!keysY.includes(key) || !areEqual(x[key], y[key])) {
      return false;
    }
  }

  return true;
}

export module MapEx {
  export const setOrUpdate = <K, V>(map: Map<K, V>, key: K, value: V): Map<K, V> => {
    const newMap = new Map(map);
    newMap.set(key, value);
    return newMap;
  }
}
