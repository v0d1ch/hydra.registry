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

const STORAGE_KEY = 'connectedWallet'

declare global {
  interface Window {
    cardano?: Record<string, { enable: () => Promise<{ getChangeAddress: () => Promise<string> }> }>
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

  useEffect(() => {
    setAvailable(getAvailableWallets())
    const saved = localStorage.getItem(STORAGE_KEY)
    if (saved) {
      connect(saved).catch(() => localStorage.removeItem(STORAGE_KEY))
    }
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  const connect = useCallback(async (walletName: string) => {
    const api = await window.cardano![walletName].enable()
    const addr = await api.getChangeAddress()
    setAddress(addr)
    localStorage.setItem(STORAGE_KEY, walletName)
  }, [])

  const disconnect = useCallback(() => {
    setAddress(null)
    localStorage.removeItem(STORAGE_KEY)
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
