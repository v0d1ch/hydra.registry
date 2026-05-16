const BASE_URL = import.meta.env.VITE_API_BASE_URL ?? ''

// ─── Existing types ───

export interface StatsResponse {
  headCount: number
  totalUtxos: number
  headsByStatus: Record<string, number>
  explorerHeadCount: number
  uniqueParticipants: number
  headsByNetwork: Record<string, number>
  totalCommittedLovelace: number
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
  headId: string
  receiverOnChainId: string
  paymentHash: string
  amountLovelace: number
  memo?: string
  expiresInSeconds?: number
}

export interface InvoiceResponse {
  invoiceId: string
  headId: string
  receiverOnChainId: string
  paymentHash: string
  amountLovelace: number
  memo: string | null
  status: string
  expiresAt: string
  createdAt: string
}

export interface FindRoutesRequest {
  invoiceId: string
  senderOnChainId: string
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
  senderAddress: string
  receiverAddress: string
  htlcStatus: string
  htlcTxHash: string | null
  secretHash: string
  preimage: string | null
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

export interface HealthResponse {
  status: string
  headCount: number
  dbConnected: boolean
  chainSlotKnown: boolean
  nodeSyncProgress: number | null
}

export function getHealth(): Promise<HealthResponse> {
  return request<HealthResponse>('/api/v1/health')
}

export function checkHead(host: string, port: number): Promise<CheckHeadResponse> {
  const params = new URLSearchParams({ host, port: String(port) })
  return request<CheckHeadResponse>(`/api/v1/heads/check?${params}`)
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

export function registerHead(host: string, port: number, walletAddress?: string): Promise<RegisterHeadResponse> {
  return request<RegisterHeadResponse>('/api/v1/heads/register', {
    method: 'POST',
    body: JSON.stringify({ host, port, walletAddress: walletAddress ?? null }),
  })
}

// ─── Registered head detail ───

export interface RegisteredHeadDetail {
  headId: string
  host: string
  port: number
  status: string
  utxoCount: number
  registeredAt: string
  lastSeenAt: string | null
  htlcEnabled: boolean
  refScriptUtxo: string | null
  onChain: ExplorerOnChain | null
}

export interface ExplorerOnChain {
  network: string
  networkMagic: number
  status: string
  snapshotNumber: number | null
  htlcEnabled: boolean
}

export function getRegisteredHead(headId: string): Promise<RegisteredHeadDetail> {
  return request<RegisteredHeadDetail>(`/api/v1/heads/${headId}`)
}

// ─── Relay graph ───

export interface SubgraphNode {
  headId: string
  network: string
  hasHtlc: boolean
  isUserHead: boolean
  participants: string[]
  committedLovelace: number
}

export interface SubgraphEdge {
  fromHead: string
  toHead: string
  bridgeAddress: string
  fee: number
}

export interface SubgraphResponse {
  nodes: SubgraphNode[]
  edges: SubgraphEdge[]
}

export function getRelayGraph(network: string): Promise<SubgraphResponse> {
  const params = new URLSearchParams({ network })
  return request<SubgraphResponse>(`/api/v1/relay/graph?${params}`)
}

export function submitPreimage(paymentHash: string, preimage: string): Promise<{ message: string }> {
  return request<{ message: string }>(`/api/v1/relay/preimage/${paymentHash}`, {
    method: 'POST',
    body: JSON.stringify({ preimage }),
  })
}

// ─── HTLC tx blueprints ───

export interface HtlcValidatorResponse {
  scriptHash: string
  scriptCborHex: string
  scriptType: string
}

export interface HtlcDatumView {
  paymentHash: string
  timeoutSlot: number
  senderPkh: string
  receiverPkh: string
}

export interface LockTxBlueprint {
  headId: string
  scriptAddress: string
  scriptHash: string
  datum: HtlcDatumView
  datumCborHex: string
  validatorRefScriptCborHex: string
  lockAmountLovelace: number
  validityUpperSlot: number
  requiredSignerPkh: string
}

export interface ClaimTxBlueprint {
  headId: string
  htlcInputTxHash: string
  htlcInputIndex: number
  redeemerCborHex: string
  validityUpperSlot: number
  requiredSignerPkh: string
}

export interface RefundTxBlueprint {
  headId: string
  htlcInputTxHash: string
  htlcInputIndex: number
  redeemerCborHex: string
  validityLowerSlot: number
  requiredSignerPkh: string
}

export function getHtlcValidator(): Promise<HtlcValidatorResponse> {
  return request<HtlcValidatorResponse>('/api/v1/htlc/validator')
}

export function getLockTxBlueprint(routeId: string, hopIndex: number): Promise<LockTxBlueprint> {
  return request<LockTxBlueprint>(`/api/v1/relay/payments/${routeId}/hops/${hopIndex}/lock-tx`, {
    method: 'POST',
  })
}

export function getClaimTxBlueprint(routeId: string, hopIndex: number, preimage: string): Promise<ClaimTxBlueprint> {
  return request<ClaimTxBlueprint>(`/api/v1/relay/payments/${routeId}/hops/${hopIndex}/claim-tx`, {
    method: 'POST',
    body: JSON.stringify({ preimage }),
  })
}

export function getRefundTxBlueprint(routeId: string, hopIndex: number): Promise<RefundTxBlueprint> {
  return request<RefundTxBlueprint>(`/api/v1/relay/payments/${routeId}/hops/${hopIndex}/refund-tx`, {
    method: 'POST',
  })
}

// ─── Explorer stats ───

export interface ExplorerStatsResponse {
  explorerHeadCount: number
  uniqueParticipants: number
  totalCommittedLovelace: number
}

export function getExplorerStats(status?: string, network?: string): Promise<ExplorerStatsResponse> {
  const params = new URLSearchParams()
  if (status) params.set('status', status)
  if (network && network !== 'All') params.set('network', network)
  const qs = params.toString()
  return request<ExplorerStatsResponse>(`/api/v1/explorer/stats${qs ? `?${qs}` : ''}`)
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

export function getPendingInvoices(): Promise<InvoiceResponse[]> {
  return request<InvoiceResponse[]>('/api/v1/relay/invoices?status=pending')
}

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

// ─── Dashboard / participant routes ───

export interface BuildResult {
  cborHex: string
  txId: string
  envelope: unknown
}

export interface SubmitResult {
  status: string   // "TxValid" | "TxInvalid" | "submitted"
  txId?: string
  error?: string
}

export interface ParticipantAction {
  hopIndex: number
  kind: string   // 'lock' | 'claim' | 'refund'
  urgency: string  // 'ok' | 'soon' | 'expiring' | 'expired'
}

export interface ParticipantRouteSummary {
  route: PaymentStatusResponse
  roles: string[]
  actions: ParticipantAction[]
}

export function getParticipantRoutes(pkh: string): Promise<ParticipantRouteSummary[]> {
  return request<ParticipantRouteSummary[]>(`/api/v1/relay/participants/${pkh}/routes`)
}

export function getParticipantInvoices(pkh: string): Promise<InvoiceResponse[]> {
  return request<InvoiceResponse[]>(`/api/v1/relay/participants/${pkh}/invoices`)
}

export function buildLockTx(routeId: string, hopIndex: number, walletAddress: string): Promise<BuildResult> {
  return request<BuildResult>(`/api/v1/relay/payments/${routeId}/hops/${hopIndex}/lock-tx-cbor`, {
    method: 'POST',
    body: JSON.stringify({ walletAddress }),
  })
}

export function buildClaimTx(routeId: string, hopIndex: number, walletAddress: string, preimage: string): Promise<BuildResult> {
  return request<BuildResult>(`/api/v1/relay/payments/${routeId}/hops/${hopIndex}/claim-tx-cbor`, {
    method: 'POST',
    body: JSON.stringify({ walletAddress, preimage }),
  })
}

export function buildRefundTx(routeId: string, hopIndex: number, walletAddress: string): Promise<BuildResult> {
  return request<BuildResult>(`/api/v1/relay/payments/${routeId}/hops/${hopIndex}/refund-tx-cbor`, {
    method: 'POST',
    body: JSON.stringify({ walletAddress }),
  })
}

export function submitTx(headId: string, signedCborHex: string): Promise<SubmitResult> {
  return request<SubmitResult>(`/api/v1/heads/${headId}/submit`, {
    method: 'POST',
    body: JSON.stringify({ signedCborHex }),
  })
}

export function getUserKeyHash(walletAddress: string): Promise<{ keyHash: string | null }> {
  return request<{ keyHash: string | null }>(`/api/v1/users/${encodeURIComponent(walletAddress)}/keyhash`)
}

// ─── Claim ownership ───

export interface ClaimOwnershipResponse {
  verified: boolean
  keyHash: string
}

export function claimOwnership(headId: string, walletAddress: string): Promise<ClaimOwnershipResponse> {
  return request<ClaimOwnershipResponse>(`/api/v1/heads/${headId}/claim-ownership`, {
    method: 'POST',
    body: JSON.stringify({ walletAddress }),
  })
}

export function setUserKeyHash(walletAddress: string, keyHash: string): Promise<{ keyHash: string | null }> {
  return request<{ keyHash: string | null }>(`/api/v1/users/${encodeURIComponent(walletAddress)}/keyhash`, {
    method: 'PUT',
    body: JSON.stringify({ keyHash }),
  })
}
