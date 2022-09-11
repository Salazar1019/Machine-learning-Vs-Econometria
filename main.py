import scrapy

class QuoteSpider( scrapy.Spider ):
   name = 'quotes'
   
   def start_requests( self ):
      urls = [
         'https://quotes.toscrape.com/page/1/',
         'https://quotes.toscrape.com/page/2/',
      ]
   
