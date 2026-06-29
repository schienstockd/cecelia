<script setup lang="ts">
import { onMounted } from 'vue'
import { useWsStore } from './stores/ws'
import AppHeader from './components/AppHeader.vue'
import AppSidebar from './components/AppSidebar.vue'
import ErrorConsole from './components/ErrorConsole.vue'

const ws = useWsStore()
onMounted(() => ws.connect())
</script>

<template>
  <div class="cc-dark cc-shell">
    <AppHeader />
    <div class="cc-content">
      <AppSidebar />
      <main class="cc-main">
        <RouterView v-slot="{ Component }">
          <KeepAlive include="ChainModule">
            <component :is="Component" />
          </KeepAlive>
        </RouterView>
      </main>
    </div>
    <ErrorConsole />
  </div>
</template>

<style scoped>
.cc-shell {
  display: flex;
  flex-direction: column;
  height: 100vh;
  overflow: hidden;
}

.cc-content {
  flex: 1;
  display: flex;
  overflow: hidden;
}

.cc-main {
  flex: 1;
  overflow-y: auto;
  background: var(--cc-bg);
}
</style>
