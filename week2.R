
data(mpg)
#?mpg
mpg %>% summary()

mpg %>%
  ggplot()+geom_point(aes(x=displ, y=hwy))

mpg %>%
  ggplot()+geom_point(aes(x=hwy, y=cyl))

mpg %>%
  ggplot()+geom_boxplot(aes(x=hwy, y=cyl, group=cyl))

mpg %>%
  ggplot()+geom_boxplot(aes(x=hwy, y=factor(cyl), group=factor(cyl)))


mpg %>%
  ggplot()+geom_point(aes(x=class, y=drv))

mpg %>%
  ggplot(aes(x=class, y=drv))+
  geom_count()

mpg %>%
  count(class, drv) %>%
  ggplot(aes(x=class, y=drv))+
  geom_tile(aes(fill=n))

mpg %>%
  ggplot()+geom_point(aes(x=displ, y=hwy, color=displ<5))

mpg %>%
  ggplot()+
  geom_point(aes(x=displ, y=hwy, color=class))

mpg %>%
  ggplot()+
  geom_point(aes(x=displ, y=hwy))+
  facet_wrap(~class, nrow=5)

mpg %>%
  ggplot()+
  geom_point(aes(x=displ, y=hwy))+
  facet_grid(~class)

mpg %>%
  ggplot()+
  geom_point(aes(x=displ, y=hwy))+
  facet_grid(class~.)

mpg %>%
  ggplot()+
  geom_point(aes(x=displ, y=hwy))+
  facet_grid(drv~cyl)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)

