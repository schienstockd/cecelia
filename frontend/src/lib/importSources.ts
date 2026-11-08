// The places images can come from other than a file on disk — one entry per source, rendered as the
// "Import from…" menu on Manage images. Adding a source is an entry here plus its dialog, registered
// by `id` in ManageImagesModule; the button row never grows.
//
// Each dialog follows the same scan → register-into-the-set → run-a-task shape
// (`LegacyMigrateDialog`, `ImmunemapImportDialog`).

export interface ImportSource {
  id: 'legacy' | 'immunemap'
  label: string        // the menu item: the source, not a verb ("Import from…" is the button)
  icon: string         // PrimeIcons class
  tip: string          // one line: what comes across
}

export const IMPORT_SOURCES: readonly ImportSource[] = [
  { id: 'legacy', label: 'Legacy Cecelia project', icon: 'pi pi-history',
    tip: 'Images, segmentation and tracking from an old (R/Shiny) Cecelia project' },
  { id: 'immunemap', label: 'Immunemap', icon: 'pi pi-cloud-download',
    tip: 'Videos from a public Immunemap experiment, with their conditions' },
]
