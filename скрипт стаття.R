################################################################################
######################### Завантаження даних та бібліотек ######################
################################################################################
library(tidyverse) # Для аналізу та побудови графічного відображення
library(kableExtra) # Для виводу таблиць на полотно
library(writexl) # Для збережння даних в excel

# Робоча директорію, яка містить датасет
setwd(choose.dir())

# Файли в папці
list.files()

# Завантаження датасету
data <- read.csv('games.csv')

################################################################################
######################### Базовий перегляд даних ###############################
################################################################################
data %>% dim() # Розмірність даних 85103 спостережень та 39 стовпців
data %>% str() # Структура даних
data %>% names() # Назви стовпців
data %>% is.na() %>% sum() # Сума пропущених значень
data %>% is.na() %>% colSums() # Пропущені значення по змінним
# data <- data[,-25] # Видалим змінну Score.rank

# Виведемо розшифрування змінних
data.frame(`Назва` = names(data), `Пояснення` = c('ID гри','Назва гри','Дата релізу','Кількість проданих копій','Найвище число одночасних користувачів (Peak Concurrent Users)',
                                                  'Мінімальний вік, необхідний для гри','Ціна гри','Кількість доступних додатків (DLC) для гри','Опис гри','Список мов, які підтримуються грою',
                                                  'Список мов, для яких доступно повне аудіо','Відгуки користувачів','Посилання на зображення заголовку гри','Посилання на веб-сайт гри',
                                                  'Посилання на веб-сайт підтримки гри','Email для підтримки гри','Чи підтримується гра на операційних системах Windows','Чи підтримується гра на операційних системах Mac','Чи підтримується гра на операційних системах Linux',
                                                  'Оцінка на Metacritic','Посилання на Metacritic','Оцінка користувачів','Кількість позитивних відгуків','Кількість негативних відгуків','Рейтинг оцінки',
                                                  'Кількість досягнень в грі','Кількість рекомендацій гри','Примітки','Середній час гри за весь час','Середній час гри за останні два тижні','Медіанний час гри за весь час','Медіанний час гри за останні два тижні',
                                                  'Розробники гри','Видавці гри','Категорії гри','Жанри гри','Теги гри','Посилання на знімки екрану гри','Посилання на відеоролики гри')) %>% kable() %>% kable_styling()

################################################################################
######################### Фільтрація та зміна типів даних ######################
################################################################################
# Відфільтруємо дані, по безкоштовним іграх та тих, які не продавались
table(data$Estimated.owners)
data2 <- data %>% filter(Estimated.owners != '0 - 0' & Price != 0 & Average.playtime.forever != 0)


# Змінимо типи даних
# Estimated.owners 
data2 <- data2 %>% mutate(Estimated.owners.factor = case_when(
  Estimated.owners == '0 - 20000' ~ 1,
  Estimated.owners == '20000 - 50000' ~ 2,
  Estimated.owners == '50000 - 100000' ~ 3,
  Estimated.owners == '100000 - 200000' ~ 4,
  Estimated.owners == '200000 - 500000' ~ 5,
  Estimated.owners == '500000 - 1000000' ~ 6,
  Estimated.owners == '1000000 - 2000000' ~ 7,
  Estimated.owners == '2000000 - 5000000' ~ 8,
  Estimated.owners == '5000000 - 10000000' ~ 9,
  Estimated.owners == '10000000 - 20000000' ~ 10,
  Estimated.owners == '20000000 - 50000000' ~ 11,
  Estimated.owners == '50000000 - 100000000' ~ 12,
)) 
data2$Estimated.owners.factor <- as.factor(data2$Estimated.owners.factor)

data2 %>% str()

# Windows, Mac, Linux
data2$Windows <- as.logical(data2$Windows)
data2$Mac <- as.logical(data2$Mac)
data2$Linux <- as.logical(data2$Linux)

################################################################################
######################### Перетворення в бінарні значення ######################
################################################################################
# Функція для отримання унікальних значень зі списку рядків
get_unique <- function(values) {
  split_ <- strsplit(values, ",")
  unique_ <- unique(unlist(split_))
  return(unique_)
}

# Створення бінарних змінних жанрів
unique_genres <- get_unique(data2$Genres)
print(unique_genres)
has_genre <- function(genres, genre) {
  grepl(genre, genres)
}
for (genre in unique_genres) {
  data2[[genre]] <- as.integer(has_genre(data2$Genres, genre))
}

# Створення бінарних змінних категорій
unique_categories <- get_unique(data2$Categories)
print(unique_categories)
has_category <- function(categories, category) {
  grepl(category, categories)
}
for (category in unique_categories) {
  data2[[category]] <- as.integer(has_category(data2$Categories, category))
}


# Створення бінарних змінних тегів
unique_tags <- get_unique(data2$Tags)
print(unique_tags)
has_tag <- function(tags, tag) {
  grepl(tag, tags)
}
for (tag in unique_tags) {
  data2[[tag]] <- as.integer(has_tag(data2$Tags, tag))
}

################################################################################
######################### Відбір даних для модель ##############################
################################################################################
data_for_fit <- data2[,c(40:ncol(data2))]

# Перевірка даних на цілісність
sum(is.na(data_for_fit))

################################################################################
######################### Побудова моделі на навальних даних ###################
################################################################################
# Зробимо Estimated.owners.factor бінарної. 1 - проект успішний (кількість проданих копій > 500 000), 0 - не успшний
data_for_fit$Estimated.owners.factor <- as.integer(data_for_fit$Estimated.owners.factor)
data_for_fit <- data_for_fit %>% mutate(Estimated.owners.factor = case_when(
  Estimated.owners.factor < 6 ~ 0,
  Estimated.owners.factor >= 6 ~ 1
)) 

# Розбиття на тестову та навальну вибірку
set.seed(123) # Задаємо seed для відтворюваності результатів
train_index <- sample(1:nrow(data_for_fit), 0.7 * nrow(data_for_fit)) # 70% даних у тренувальному наборі
train_data <- data_for_fit[train_index, ]
test_data <- data_for_fit[-train_index, ]

# Побудова логістичної моделі на навчальні дані
fit_ <- glm(Estimated.owners.factor ~., data = train_data, family = binomial)

# Підсумок моделі
summary(fit_)

# Прогноз
predicted_values <- predict(fit_, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_values > 0.5, 1, 0) 
accuracy <- mean(predicted_classes == as.integer(as.character(test_data$Estimated.owners.factor))) * 100
accuracy

################################################################################
######################### Побудова моделі ######################################
################################################################################

table(data2$Estimated.owners)


fit_1 <- glm(Estimated.owners.factor ~., data = data_for_fit, family = binomial)
summary(fit_1)

predicted_values1 <- predict(fit_1, newdata = data_for_fit, type = "response")
predicted_classes10 <- ifelse(predicted_values1 > 0.1, 1, 0) 
predicted_classes20 <- ifelse(predicted_values1 > 0.2, 1, 0) 
predicted_classes30 <- ifelse(predicted_values1 > 0.3, 1, 0) 
predicted_classes40 <- ifelse(predicted_values1 > 0.4, 1, 0)
predicted_classes50 <- ifelse(predicted_values1 > 0.5, 1, 0) 
predicted_classes60 <- ifelse(predicted_values1 > 0.6, 1, 0) 
predicted_classes2 <- ifelse(predicted_values1 > 0.02, 1, 0) 
predicted_classes1 <- ifelse(predicted_values1 > 0.01, 1, 0) 
predicted_classes0 <- ifelse(predicted_values1 > 0, 1, 0)
predicted_classes5 <- ifelse(predicted_values1 > 0.05, 1, 0) 



confusion_matrix10 <- table(Actual = data_for_fit$Estimated.owners.factor, Predicted =predicted_classes10)
accuracy10 <- sum(diag(confusion_matrix10)) / sum(confusion_matrix10)
print(confusion_matrix10)
print(paste("Якість моделі10:", accuracy10))

confusion_matrix20 <- table(Actual = data_for_fit$Estimated.owners.factor, Predicted =predicted_classes20)
accuracy20 <- sum(diag(confusion_matrix20)) / sum(confusion_matrix20)
print(confusion_matrix20)
print(paste("Якість моделі20:", accuracy20))

confusion_matrix30 <- table(Actual = data_for_fit$Estimated.owners.factor, Predicted =predicted_classes30)
accuracy30 <- sum(diag(confusion_matrix30)) / sum(confusion_matrix30)
print(confusion_matrix30)
print(paste("Якість моделі30:", accuracy30))

confusion_matrix40 <- table(Actual = data_for_fit$Estimated.owners.factor, Predicted =predicted_classes40)
accuracy40 <- sum(diag(confusion_matrix40)) / sum(confusion_matrix40)
print(confusion_matrix40)
print(paste("Якість моделі40:", accuracy40))

confusion_matrix50 <- table(Actual = data_for_fit$Estimated.owners.factor, Predicted =predicted_classes50)
accuracy50 <- sum(diag(confusion_matrix50)) / sum(confusion_matrix50)
print(confusion_matrix50)
print(paste("Якість моделі50:", accuracy50))

confusion_matrix60 <- table(Actual = data_for_fit$Estimated.owners.factor, Predicted =predicted_classes60)
accuracy60 <- sum(diag(confusion_matrix60)) / sum(confusion_matrix60)
print(confusion_matrix60)
print(paste("Якість моделі60:", accuracy60))

confusion_matrix40 <- table(Actual = data_for_fit$Estimated.owners.factor, Predicted =predicted_classes40)
accuracy40 <- sum(diag(confusion_matrix40)) / sum(confusion_matrix40)
print(confusion_matrix40)
print(paste("Якість моделі40:", accuracy40))

confusion_matrix5 <- table(Actual = data_for_fit$Estimated.owners.factor, Predicted =predicted_classes5)
accuracy5 <- sum(diag(confusion_matrix5)) / sum(confusion_matrix5)
print(confusion_matrix5)
print(paste("Якість моделі5:", accuracy5))

confusion_matrix2 <- table(Actual = data_for_fit$Estimated.owners.factor, Predicted =predicted_classes2)
accuracy2 <- sum(diag(confusion_matrix2)) / sum(confusion_matrix2)
print(confusion_matrix2)
print(paste("Якість моделі2:", accuracy2))

confusion_matrix1 <- table(Actual = data_for_fit$Estimated.owners.factor, Predicted =predicted_classes1)
accuracy1 <- sum(diag(confusion_matrix1)) / sum(confusion_matrix1)
print(confusion_matrix1)
print(paste("Якість моделі1:", accuracy1))

confusion_matrix0 <- table(Actual = data_for_fit$Estimated.owners.factor, Predicted =predicted_classes0)
accuracy0 <- sum(diag(confusion_matrix0)) / sum(confusion_matrix0)
print(confusion_matrix0)
print(paste("Якість моделі0:", accuracy0))