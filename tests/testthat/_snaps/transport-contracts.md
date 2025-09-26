# tokens_create returns consistent schema across transports

    Code
      rest_tbl
    Output
      # A tibble: 1 x 7
        transaction_id  status receipt_status consensus_timestamp receipt response    
        <chr>           <chr>  <chr>          <dttm>              <list>  <list>      
      1 0.0.5001-17000~ SUCCE~ <NA>           NA                  <list>  <named list>
      # i 1 more variable: token_id <chr>

