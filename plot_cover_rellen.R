library('ggplot2')
library('reshape2')
library('gridExtra')

cover80 <- read.csv('cover80.csv', row.names=1)
cover95 <- read.csv('cover95.csv', row.names=1)
len80 <- read.csv('len80.csv', row.names=1)
len95 <- read.csv('len95.csv', row.names=1)


rellen80 <- len80 * (cover80 > 0.8)
rellen80[rellen80 == 0] <- NA
rellen80 <- len80 / apply(rellen80, 1, min, na.rm=T)
rellen80 <- melt(rellen80)
names(rellen80) <- c('method', 'rel_len')
rellen80['fraction'] <- rownames(len80)

cover_rellen80 <- cbind(melt(cover80), melt(rellen80))
cover_rellen80 <- cover_rellen80[ , c(1:2, 4, 6)]
names(cover_rellen80) <- c('method', 'cover', 'fraction', 'rel_len')

plt_cover_rellen80 <- ggplot(cover_rellen80, aes(x=rel_len, y=cover, color=method, shape=method)) +
  scale_shape_manual(values=1:7) +
  geom_point() +
  geom_hline(yintercept=0.8, col='black') +
  theme_bw() +
  scale_x_continuous(n.breaks=6) +
  labs(x='Relative Length', y='Coverage Rate', color='Method', shape='Method', title='80% prediction intervals') +
  theme(plot.title=element_text(hjust=0.5))


rellen95 <- len95 * (cover95 > 0.95)
rellen95[rellen95 == 0] <- NA
rellen95 <- len95 / apply(rellen95, 1, min, na.rm=T)
rellen95 <- melt(rellen95)
names(rellen95) <- c('method', 'rel_len')
rellen95['fraction'] <- rownames(len95)

cover_rellen95 <- cbind(melt(cover95), melt(rellen95))
cover_rellen95 <- cover_rellen95[ , c(1:2, 4, 6)]
names(cover_rellen95) <- c('method', 'cover', 'fraction', 'rel_len')

plt_cover_rellen95 <- ggplot(cover_rellen95, aes(x=rel_len, y=cover, color=method, shape=method)) +
  scale_shape_manual(values=1:7) +
  geom_point() +
  geom_hline(yintercept=0.95, col='black') +
  theme_bw() +
  scale_x_continuous(n.breaks=6) +
  labs(x='Relative Length', y='Coverage Rate', color='Method', shape='Method', title='95% prediction intervals') +
  theme(plot.title=element_text(hjust=0.5))

grid.arrange(plt_cover_rellen80, plt_cover_rellen95, nrow=2)