import React from "react"
import { useCookies } from "react-cookie"
import { CookieSetOptions } from "universal-cookie"

export function useCookieState(fields: string[], setOptions: CookieSetOptions) {
  const [cookies, setCookie, clearCookie] = useCookies(fields)
  return fields.map(field => [
    cookies[field],
    (value: any) => setCookie(field, value, setOptions),
    () => clearCookie(field, setOptions),
  ])
}

export function changeEventSetter<T>(setter: (value: string) => void) {
  return (ev: React.ChangeEvent<HTMLInputElement>) => {
    setter(ev.target.value)
  }
}
