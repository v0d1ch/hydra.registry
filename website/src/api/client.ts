const BASE_URL = import.meta.env.VITE_API_BASE_URL ?? ''

// ─── Existing types ───

export interface StatsResponse {
  headCount: number
  totalUtxos: number
  headsByStatus: Record<string, number>
  explorerHeadCount: number
}

export interface HeadInfo {
  headId: string
  host: string
  port: number
  status: string
}

export interface RegisterHeadResponse {
  headId: string
  status: string
}

// ─── Explorer types ───

export interface ExplorerHeadInfo {
  headId: string
  network: string
  networkMagic: number
  version: string
  status: string
  contestationPeriod: number | null
  contestations: number | null
  snapshotNumber: number | null
  contestationDeadline: string | null
  point: unknown | null
  blockNo: number | null
  members: unknown | null
  seedTxIn: string | null
  firstSeenAt: string
  lastUpdatedAt: string
  registered: boolean
  htlcEnabled: boolean
}

// ─── Participant types ───

export interface ParticipantHeadInfo {
  headId: string
  address: string
  vkey: string | null
  onChainId: string | null
  committedLovelace: number
  committedTxRef: string | null
  headStatus: string
  network: string
}

// ─── Relay types ───

export interface CreateInvoiceRequest {
  receiverAddress: string
  paymentHash: string
  amountLovelace: number
  memo?: string
  expiresInSeconds?: number
}

export interface InvoiceResponse {
  invoiceId: string
  receiverAddress: string
  paymentHash: string
  amountLovelace: number
  memo: string | null
  status: string
  expiresAt: string
  createdAt: string
}

export interface FindRoutesRequest {
  invoiceId: string
  senderAddress: string
  network: string
}

export interface RouteResponse {
  routeId: string
  hops: RouteHopResponse[]
  totalFee: number
}

export interface RouteHopResponse {
  headId: string
  bridgeAddress: string
  fee: number
}

export interface PaymentStatusResponse {
  routeId: string
  invoiceId: string
  senderAddress: string
  receiverAddress: string
  amountLovelace: number
  status: string
  totalFee: number
  network: string
  hops: HopStatusResponse[]
  createdAt: string
  updatedAt: string
}

export interface HopStatusResponse {
  hopIndex: number
  headId: string
  bridgeAddress: string
  htlcStatus: string
  htlcTxHash: string | null
  secretHash: string
  timeoutSlot: number
  fee: number
  lockedAt: string | null
  claimedAt: string | null
}

// ─── Check head types ───

export interface CheckHeadResponse {
  headId: string
  headStatus: string
  alreadyRegistered: boolean
}

export function checkHead(host: string, port: number): Promise<CheckHeadResponse> {
  const params = new URLSearchParams({ host, port: String(port) })
  return request<CheckHeadResponse>(`/api/v1/heads/check?${params}`)
}

// ─── Deposit types ───

export interface DepositRequest {
  host: string
  port: number
  network: string
}

export interface DepositResponse {
  depositTxCbor: string
  message: string
}

// ─── Request helper ───

async function request<T>(path: string, init?: RequestInit): Promise<T> {
  const res = await fetch(`${BASE_URL}${path}`, {
    headers: { 'Content-Type': 'application/json' },
    ...init,
  })
  if (!res.ok) {
    const body = await res.json().catch(() => null)
    throw new Error(body?.error ?? `API error ${res.status}`)
  }
  return res.json() as Promise<T>
}

// ─── Stats & Heads ───

export function getStats(): Promise<StatsResponse> {
  return request<StatsResponse>('/api/v1/stats')
}

export function getHeads(count?: number, page?: number): Promise<HeadInfo[]> {
  const params = new URLSearchParams()
  if (count !== undefined) params.set('count', String(count))
  if (page !== undefined) params.set('page', String(page))
  const qs = params.toString()
  return request<HeadInfo[]>(`/api/v1/heads${qs ? `?${qs}` : ''}`)
}

export function registerHead(
  host: string,
  port: number,
  bridge?: boolean,
  feeLovelace?: number
): Promise<RegisterHeadResponse> {
  return request<RegisterHeadResponse>('/api/v1/heads/register', {
    method: 'POST',
    body: JSON.stringify({ host, port, bridge, feeLovelace }),
  })
}

// ─── Deposit ───

export function requestDeposit(req: DepositRequest): Promise<DepositResponse> {
  return request<DepositResponse>('/api/v1/heads/deposit', {
    method: 'POST',
    body: JSON.stringify(req),
  })
}

// ─── Explorer ───

export function getExplorerHeads(
  count?: number,
  page?: number,
  status?: string,
  network?: string
): Promise<ExplorerHeadInfo[]> {
  const params = new URLSearchParams()
  if (count !== undefined) params.set('count', String(count))
  if (page !== undefined) params.set('page', String(page))
  if (status) params.set('status', status)
  if (network && network !== 'All') params.set('network', network)
  const qs = params.toString()
  return request<ExplorerHeadInfo[]>(`/api/v1/explorer/heads${qs ? `?${qs}` : ''}`)
}

export function getExplorerHead(headId: string): Promise<ExplorerHeadInfo> {
  return request<ExplorerHeadInfo>(`/api/v1/explorer/heads/${headId}`)
}

// ─── UTxO types (Blockfrost-compatible) ───

export interface UtxoAmount {
  unit: string
  quantity: string
}

export interface UtxoResponse {
  address: string
  tx_hash: string
  output_index: number
  amount: UtxoAmount[]
  data_hash: string | null
  inline_datum: unknown | null
  reference_script_hash: string | null
  head_id: string
}

// ─── Participants ───

export function getHeadsByAddress(address: string): Promise<ParticipantHeadInfo[]> {
  return request<ParticipantHeadInfo[]>(`/api/v1/addresses/${address}/heads`)
}

export function getAddressUtxos(address: string): Promise<UtxoResponse[]> {
  return request<UtxoResponse[]>(`/addresses/${address}/utxos`)
}

export function getHeadAddresses(headId: string): Promise<string[]> {
  return request<string[]>(`/api/v1/heads/${headId}/addresses`)
}

export function getHeadParticipants(headId: string): Promise<ParticipantHeadInfo[]> {
  // Re-use: each participant row includes the head info
  return request<ParticipantHeadInfo[]>(`/api/v1/explorer/heads/${headId}/participants`)
}

// ─── Relay: Invoices ───

export function createInvoice(req: CreateInvoiceRequest): Promise<InvoiceResponse> {
  return request<InvoiceResponse>('/api/v1/relay/invoices', {
    method: 'POST',
    body: JSON.stringify(req),
  })
}

export function getInvoice(invoiceId: string): Promise<InvoiceResponse> {
  return request<InvoiceResponse>(`/api/v1/relay/invoices/${invoiceId}`)
}

// ─── Relay: Routes ───

export function findRoutes(req: FindRoutesRequest): Promise<RouteResponse[]> {
  return request<RouteResponse[]>('/api/v1/relay/routes', {
    method: 'POST',
    body: JSON.stringify(req),
  })
}

export function executeRoute(routeId: string): Promise<PaymentStatusResponse> {
  return request<PaymentStatusResponse>(`/api/v1/relay/routes/${routeId}/execute`, {
    method: 'POST',
  })
}

// ─── Relay: Payments ───

export function getPaymentStatus(paymentId: string): Promise<PaymentStatusResponse> {
  return request<PaymentStatusResponse>(`/api/v1/relay/payments/${paymentId}`)
}
