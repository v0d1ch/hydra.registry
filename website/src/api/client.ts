const BASE_URL = import.meta.env.VITE_API_BASE_URL ?? ''

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

export function registerHead(host: string, port: number): Promise<RegisterHeadResponse> {
  return request<RegisterHeadResponse>('/api/v1/heads/register', {
    method: 'POST',
    body: JSON.stringify({ host, port }),
  })
}
