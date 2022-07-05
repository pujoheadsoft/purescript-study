module CleanArchitecture.Port.ArticlePort where


import CleanArchitecture.Domain.Article (ArticleTitle, Articles)

type ArticlePort = {
  findArticlesByTitle :: (ArticleTitle -> Articles)
}