import { createContext, useContext, useState } from 'react'
import type { InvoiceResponse } from '../api/client'

interface UserContextValue {
  pendingInvoice: InvoiceResponse | null
  setPendingInvoice: (inv: InvoiceResponse) => void
}

const UserContext = createContext<UserContextValue>({
  pendingInvoice: null,
  setPendingInvoice: () => {},
})

export function UserProvider({ children }: { children: React.ReactNode }) {
  const [pendingInvoice, setPendingInvoice] = useState<InvoiceResponse | null>(null)

  return (
    <UserContext.Provider value={{ pendingInvoice, setPendingInvoice }}>
      {children}
    </UserContext.Provider>
  )
}

export function useUser() {
  return useContext(UserContext)
}
