use insites_metrics_dev;

select * from meta.equity_price where capiq_symbol_ticker = 'GOOGL';	


select top 1000 * from opportunity.global_market_mover_equity WHERE capiq_company_id = 29096;

SELECT top 1000 * FROM meta.company where company_name like '%alpha%' and capiq_company_id = 29096;

select * from quandl.cur_aud