module CleanArchitecture.Usecase.FindArticleUsecase where


import CleanArchitecture.Domain.Article (Articles, ArticleTitle)

findArticles :: ArticleTitle -> Articles
findArticles title = [{title: "", body: "", author: ""}]