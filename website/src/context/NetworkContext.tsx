import { createContext, useContext, useState, type ReactNode } from 'react'

export type Network = 'Mainnet' | 'Testnet' | 'All'

interface NetworkContextType {
  network: Network
  setNetwork: (n: Network) => void
}

const NetworkContext = createContext<NetworkContextType>({
  network: 'All',
  setNetwork: () => {},
})

export function NetworkProvider({ children }: { children: ReactNode }) {
  const [network, setNetwork] = useState<Network>(
    () => (localStorage.getItem('selectedNetwork') as Network) ?? 'All'
  )

  const handleSet = (n: Network) => {
    setNetwork(n)
    localStorage.setItem('selectedNetwork', n)
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
