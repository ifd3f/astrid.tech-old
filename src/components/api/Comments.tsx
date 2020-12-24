import React from "react"
import { Button } from "reactstrap"
import { useAPI } from "./APIProvider"

export const CommentSection = () => {
  const { api, login } = useAPI()
  const onClickLogin = async () => {
    await login("astrid", "robotics")
  }
  return (
    <div>
      <Button onClick={onClickLogin}>Log In</Button>
    </div>
  )
}
