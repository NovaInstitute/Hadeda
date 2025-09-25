# tokens_create returns consistent schema across transports

    Code
      rest_tbl
    Output
      # A tibble: 1 x 7
        transaction_id  status receipt_status consensus_timestamp receipt response    
        <chr>           <chr>  <chr>          <dttm>              <list>  <list>      
      1 0.0.5001-17000~ SUCCE~ <NA>           NA                  <list>  <named list>
      # i 1 more variable: token_id <chr>

# crypto_create_account aligns REST fallback and gRPC schemas

    Code
      grpc_tbl
    Output
      # A tibble: 1 x 8
        transaction_id          status receipt_status consensus_timestamp receipt     
        <chr>                   <chr>  <chr>          <dttm>              <list>      
      1 0.0.2001-1700000001-00~ OK     SUCCESS        NA                  <named list>
      # i 3 more variables: response <list>, account_id <chr>, metadata <list>

