module CleanArchitecture.Domain.User where

type UserId = String
type UserName = String
type User = {id :: UserId, name :: UserName}
