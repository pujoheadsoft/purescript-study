module CleanArchitecture.Gateway.ArticleGateway where


import CleanArchitecture.Domain.Article (ArticleTitle, Articles)


findArticlesByTitle :: (ArticleTitle -> Articles)
findArticlesByTitle title = [{title: "", body: "", author: ""}]