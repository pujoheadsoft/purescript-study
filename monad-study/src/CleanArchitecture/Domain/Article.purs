module CleanArchitecture.Domain.Article where

type ArticleId = String
type ArticleIds = Array ArticleId
type Article = {title :: ArticleTitle, body :: ArticleBody, author :: ArticleAuthor}
type Articles = Array Article

type ArticleTitle = String
type ArticleBody = String
type ArticleAuthor = String