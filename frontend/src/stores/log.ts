import { defineStore } from 'pinia'
import { ref, computed } from 'vue'

export type LogLevel = 'info' | 'warn' | 'error'

export interface LogEntry {
  id: number
  level: LogLevel
  message: string
  detail?: string   // stack trace, server response body, etc.
  source?: string   // 'ws' | 'api' | 'import' | 'metadata' | ...
  timestamp: Date
}

let _id = 0

export const useLogStore = defineStore('log', () => {
  const entries = ref<LogEntry[]>([])
  const unreadErrors = ref(0)
  const consoleOpen = ref(false)

  function push(level: LogLevel, message: string, opts?: { detail?: string; source?: string }) {
    entries.value.push({
      id: _id++,
      level,
      message,
      detail: opts?.detail,
      source: opts?.source,
      timestamp: new Date(),
    })
    if (level === 'error' && !consoleOpen.value) unreadErrors.value++
  }

  function info(message: string, opts?: { detail?: string; source?: string }) {
    push('info', message, opts)
  }
  function warn(message: string, opts?: { detail?: string; source?: string }) {
    push('warn', message, opts)
  }
  function error(message: string, opts?: { detail?: string; source?: string }) {
    push('error', message, opts)
  }

  function openConsole() {
    consoleOpen.value = true
    unreadErrors.value = 0
  }
  function closeConsole() {
    consoleOpen.value = false
  }
  function toggleConsole() {
    if (consoleOpen.value) closeConsole()
    else openConsole()
  }

  function clear() {
    entries.value = []
    unreadErrors.value = 0
  }

  const lastEntry = computed(() =>
    entries.value.length ? entries.value[entries.value.length - 1] : null
  )

  return {
    entries, unreadErrors, consoleOpen, lastEntry,
    info, warn, error, push,
    openConsole, closeConsole, toggleConsole, clear,
  }
})
