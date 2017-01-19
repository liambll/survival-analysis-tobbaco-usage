df = read.csv('D:/OneDrive/Data/School/Canada/Course/Course Outline/STAT 645/Assignment/Assignment 1/morph.data.csv')
df_features = subset(df, select=-c(ID))
S = cov(x = df_features, y = NULL, use = "everything")
R = cov2cor(S)

eigen_S = eigen(S)
eigen_R = eigen(R)