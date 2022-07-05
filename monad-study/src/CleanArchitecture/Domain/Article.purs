module CleanArchitecture.Domain.Article where

type Article = {title :: ArticleTitle, body :: ArticleBody, author :: ArticleAuthor}
type Articles = Array Article

type ArticleTitle = String
type ArticleBody = String
type ArticleAuthor = String