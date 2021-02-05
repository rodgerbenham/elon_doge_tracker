library(httr)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(lubridate)
library(rtweet)

tweet_df <- get_timelines(c("elonmusk"), n = 1000)
tweet_df <- tweet_df %>% rowwise() %>% mutate(new_created_at = as.POSIXct(format(created_at, usetz=TRUE, tz="Australia/Melbourne")))

docol <- function(open, close) {
    if (open >= close) {
        return("gain")
    }
    return("loss")
}

for (timeframe in c("15m", "1h", "4h")) {
    GET(
        url = "https://api.binance.com",
        path = "api/v3/klines",
        query = list (
            symbol="DOGEUSDT",
            interval = timeframe
        )
    ) -> res3

    data <- content(res3, as="parsed")
    df <- data.frame(matrix(unlist(data), nrow=500, byrow=T),stringsAsFactors=FALSE)
    df <- tail(df, n=30)

    df$X1 <- as.numeric(df$X1)
    df$X2 <- as.numeric(df$X2)
    df$X3 <- as.numeric(df$X3)
    df$X4 <- as.numeric(df$X4)
    df$X5 <- as.numeric(df$X5)

    df <- df %>% rowwise() %>% mutate(colour = docol(X2, X5))
    df <- df %>% rowwise() %>% mutate(datelabel = as.POSIXct(X1 / 1000, origin = "1970-01-01", tz="GMT"))
    df <- df %>% rowwise() %>% mutate(new_datelabel = as.POSIXct(format(datelabel, usetz=TRUE, tz="Australia/Melbourne")))

    tweet_df_filt <- tweet_df %>% filter(new_created_at > df[1,]$new_datelabel)

    p <- ggplot(df) 
    p <- p + geom_vline(data=tweet_df_filt, aes(xintercept=new_created_at, size="Elon Tweet"), alpha=0.5, color="blue")
    if (timeframe == "15m") {
        p <- p + geom_errorbar(aes(x=new_datelabel + ((15*60) / 2), colour=colour, ymin=X4, ymax=X3)) 
        p <- p + geom_rect(aes(xmin=new_datelabel, xmax=(new_datelabel + 15*60), colour=colour, fill=colour, ymin=X5, ymax=X2)) 

    } else if (timeframe == "1h") {
        p <- p + geom_errorbar(aes(x=new_datelabel + ((60*60) / 2), colour=colour, ymin=X4, ymax=X3)) 
        p <- p + geom_rect(aes(xmin=new_datelabel, xmax=(new_datelabel + 60*60), colour=colour, fill=colour, ymin=X5, ymax=X2)) 
    }
    else {
        p <- p + geom_errorbar(aes(x=new_datelabel + ((60*60*4) / 2), colour=colour, ymin=X4, ymax=X3)) 
        p <- p + geom_rect(aes(xmin=new_datelabel, xmax=(new_datelabel + 60*60*4), colour=colour, fill=colour, ymin=X5, ymax=X2)) 
    }
    
    p <- p + theme(axis.title.x = element_blank(), axis.title.y=element_blank()) 
    if (timeframe == "15m") {
       p <- p + scale_x_datetime(labels = date_format("%H:%M:%S"))
    }
    else {
       p <- p + scale_x_datetime(labels = label_date_short())
    }
    p <- p + xlab("Date / Time (AEDT)")
    p <- p + ylab("$DOGE Price (USD)")
    p <- p + scale_colour_manual(name=paste0(timeframe, " Period"), labels=c("Loss", "Gain"), values=c("#FF0000", "#00FF00"))
    p <- p + scale_fill_manual(name=paste0(timeframe, " Period"), labels=c("Loss", "Gain"), values=c("#FF0000", "#00FF00"))
    p <- p + scale_size_manual(name="", values=c(0.5))
    p <- p + scale_y_continuous(breaks = breaks_extended(10))
    p <- p + theme_bw()

    ggsave(p, file=paste0(timeframe, ".pdf"), width=8, height=4.5)
    ggsave(p, file=paste0(timeframe, ".png"), width=8, height=4.5)
}
