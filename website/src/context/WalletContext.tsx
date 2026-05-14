import { createContext, useContext, useState, useEffect, useCallback } from 'react'

interface WalletContextValue {
  address: string | null
  connect: (walletName: string) => Promise<void>
  disconnect: () => void
  available: string[]
}

const WalletContext = createContext<WalletContextValue>({
  address: null,
  connect: async () => {},
  disconnect: () => {},
  available: [],
})

declare global {
  interface Window {
    cardano?: Record<string, { enable: () => Promise<{ getChangeAddress: () => Promise<string> }>; isEnabled?: () => Promise<boolean> }>
  }
}

function getAvailableWallets(): string[] {
  if (typeof window === 'undefined' || !window.cardano) return []
  return Object.keys(window.cardano).filter(k => {
    try { return typeof window.cardano![k]?.enable === 'function' } catch { return false }
  })
}

export function WalletProvider({ children }: { children: React.ReactNode }) {
  const [address, setAddress] = useState<string | null>(null)
  const [available, setAvailable] = useState<string[]>([])

  const connect = useCallback(async (walletName: string) => {
    const api = await window.cardano![walletName].enable()
    const addr = await api.getChangeAddress()
    setAddress(addr)
  }, [])

  useEffect(() => {
    const wallets = getAvailableWallets()
    setAvailable(wallets)
    // Auto-reconnect to whichever wallet is already enabled (no localStorage needed)
    ;(async () => {
      for (const name of wallets) {
        try {
          const already = await window.cardano![name].isEnabled?.()
          if (already) { await connect(name); break }
        } catch { /* ignore */ }
      }
    })()
  }, [connect])

  const disconnect = useCallback(() => {
    setAddress(null)
  }, [])

  return (
    <WalletContext.Provider value={{ address, connect, disconnect, available }}>
      {children}
    </WalletContext.Provider>
  )
}

export function useWallet() {
  return useContext(WalletContext)
}
