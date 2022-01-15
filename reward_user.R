################################### 3. 리워드 대상자 추 #############################
library(sqldf)
head(a_payment_trx)
head(dutchpay_claim)


sqldf("

    with pay_can as
    (
          select a.user_id
                ,a.transaction_id
                ,a.pay_transacted_at
                ,a.payment
                ,b.can_transacted_at
                ,case when b.cancel is null then 0 else b.cancel end as cancel
      
          from  (
                 select user_id
                      ,transaction_id 
                      ,transacted_at as pay_transacted_at
                      ,case when payment_action_type = 'PAYMENT' then amount end as payment
                      
                from a_payment_trx
                where  payment_action_type ='PAYMENT'
                ) a
          
          left join 
          
               (
                select user_id
                     ,transaction_id
                     ,transacted_at as can_transacted_at
                     ,case when payment_action_type = 'CANCEL' then 
                         case when amount is null then '0' else amount end 
                      end as cancel
                     
               from a_payment_trx
               where payment_action_type = 'CANCEL'
               
               ) b
               
          on (a.user_id = b.user_id and a.transaction_id = b.transaction_id)
         
          where pay_transacted_at between '2019-12-01' and '2019-12-31' 
    )
    
    select distinct user_id
          
    from pay_can
    
    group by user_id
    
    having sum(payment) - sum(cancel) >= 10000
   ")

 

# 총 1149명


      
      
      
      
      
      
      