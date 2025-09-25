# account parsing remains stable

    Code
      parsed
    Output
      # A tibble: 1 x 5
        account  balance timestamp           deleted public_key                 
        <chr>      <dbl> <dttm>              <lgl>   <chr>                      
      1 0.0.1001     100 2023-11-14 22:13:20 FALSE   302a300506032b6570032100...

# token balance parsing remains stable

    Code
      parsed
    Output
      # A tibble: 1 x 3
        account     balance decimals
        <chr>         <dbl>    <dbl>
      1 0.0.1001 2500000000        8

# consensus response parsing normalises chunks

    Code
      parsed
    Output
      # A tibble: 2 x 9
        topic_id chunk_number transaction_id        status receipt_status acknowledged
        <chr>           <int> <chr>                 <chr>  <chr>          <lgl>       
      1 0.0.9001            1 0.0.5001-1700000000-~ OK     SUCCESS        TRUE        
      2 0.0.9001            2 0.0.5001-1700000000-~ BUSY   BUSY           FALSE       
      # i 3 more variables: sequence_number <dbl>, consensus_timestamp <dttm>,
      #   response <list>

