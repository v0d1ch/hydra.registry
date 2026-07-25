import { createContext, useContext, useState, type ReactNode } from 'react'

export type Network = 'All' | 'Mainnet' | 'Preview' | 'Preprod'

interface NetworkContextType {
  network: Network
  setNetwork: (n: Network) => void
}

const NetworkContext = createContext<NetworkContextType>({
  network: 'All',
  setNetwork: () => {},
})

const STORAGE_KEY = 'hydra-registry-network'
const VALID_NETWORKS: Network[] = ['All', 'Mainnet', 'Preview', 'Preprod']

function loadStoredNetwork(): Network {
  try {
    const stored = localStorage.getItem(STORAGE_KEY)
    if (stored && VALID_NETWORKS.includes(stored as Network)) return stored as Network
  } catch {
    // localStorage unavailable (private mode, SSR) — fall through
  }
  return 'All'
}

export function NetworkProvider({ children }: { children: ReactNode }) {
  const [network, setNetwork] = useState<Network>(loadStoredNetwork)

  const handleSet = (n: Network) => {
    setNetwork(n)
    try {
      localStorage.setItem(STORAGE_KEY, n)
    } catch {
      // best effort — selection still works for this session
    }
  }

  return (
    <NetworkContext.Provider value={{ network, setNetwork: handleSet }}>
      {children}
    </NetworkContext.Provider>
  )
}

export function useNetwork() {
  return useContext(NetworkContext)
}
