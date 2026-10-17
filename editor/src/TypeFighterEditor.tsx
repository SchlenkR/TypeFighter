import { CSSProperties, useEffect, useRef } from "react";
import * as monaco from "monaco-editor";
import { type TypeFighterLanguageClient } from "./api";
import {
  clearModelLanguageClient,
  ensureTypefighterLanguage,
  ensureTypefighterProviders,
  refreshTypefighterTokensFromPrelude,
  setModelLanguageClient,
  typefighterOverflowWidgetsNode,
} from "./monaco";

// Deliberately thin: a Monaco instance wired for the TypeFighter language,
// nothing more. Behaviour (compile-info loop, markers, custom actions) is
// attached by the host via onMount -- see attachments.ts. onMount may return
// a disposer; it runs on unmount.
export type TypeFighterEditorInstance = monaco.editor.IStandaloneCodeEditor;

export type TypeFighterEditorProps = {
  defaultValue?: string;
  languageClient?: TypeFighterLanguageClient;
  onChange?: (value: string) => void;
  onMount?: (editor: TypeFighterEditorInstance) => void | (() => void);
  options?: monaco.editor.IStandaloneEditorConstructionOptions;
  className?: string;
  style?: CSSProperties;
};

export default function TypeFighterEditor({
  defaultValue = "",
  languageClient,
  onChange,
  onMount,
  options,
  className,
  style,
}: TypeFighterEditorProps) {
  const containerRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!containerRef.current) return;
    ensureTypefighterLanguage();
    ensureTypefighterProviders();
    refreshTypefighterTokensFromPrelude(languageClient);

    const editor = monaco.editor.create(containerRef.current, {
      value: defaultValue,
      language: "typefighter",
      theme: "typefighter-dark",
      automaticLayout: true,
      minimap: { enabled: false },
      fontSize: 13,
      scrollBeyondLastLine: false,
      tabSize: 2,
      // Hover/suggest must never clip inside modals/panels.
      fixedOverflowWidgets: true,
      overflowWidgetsDomNode: typefighterOverflowWidgetsNode(),
      ...options,
    });
    const model = editor.getModel();
    if (model) setModelLanguageClient(model, languageClient);

    const changeSub = onChange
      ? editor.onDidChangeModelContent(() => onChange(editor.getValue()))
      : null;
    const detach = onMount?.(editor);

    return () => {
      detach?.();
      changeSub?.dispose();
      if (model) clearModelLanguageClient(model);
      editor.dispose();
    };
    // Mount-once by design: defaultValue/options are initial-only, hosts drive
    // later changes imperatively through the editor instance.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  return (
    <div
      ref={containerRef}
      className={className}
      style={{ width: "100%", height: "100%", ...style }}
    />
  );
}
