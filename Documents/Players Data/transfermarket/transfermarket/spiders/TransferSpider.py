import scrapy
import re
import pandas as pd
import os


class FussballSpider(scrapy.Spider):
    name = 'run'
    
  
    with open (r"links.csv",encoding='utf-8-sig') as f:
        start_urls = [url.strip()  for url in f.readlines()]

  
    def parse(self, response):

        league = response.xpath("//header[@class='data-header']//h1/text()").get()
   
        for link in response.xpath("//div[@id='yw1']//tbody/tr/td[2]/a[1]"):
            club_link = link.xpath(".//@href").get()
            club_name = link.xpath(".//@title").get()
            yield scrapy.Request(url=f"https://www.transfermarkt.co.uk{club_link}",dont_filter=True,callback=self.detail_link,meta={'link':response.url,'club':club_name,'league':league})
      
       
    
    def detail_link(self,response):
   
        for link in response.xpath("//a[@class='tm-tab tm-tab__active--parent']/following-sibling::a[1]"):
            detail_link = link.xpath(".//@href").get()
            yield scrapy.Request(url=f"https://www.transfermarkt.co.uk{detail_link}",meta={'link':response.meta['link'],'club':response.meta['club'],'club_link':response.url,'league':response.meta['league']}, dont_filter=True,callback=self.get_info)
            
           
         

    def get_info(self,response): 

        player_indx = position_index = Birth_index = country_index = join_index = contract_index = market_index =0
        
        for ind, thead in enumerate(response.xpath("//table[@class='items']/thead/tr/th")):
            txt = thead.xpath(".//text()").extract()
            txt = ''.join(txt)

            if "player" in txt.lower():
                player_indx = ind + 1
            if "joined" in txt.lower():
                join_index = ind + 1
            if "nat." in txt.lower():
                country_index = ind + 1
            if "birth" in txt.lower():
                Birth_index = ind + 1
            if "contract" in txt.lower():
                contract_index = ind + 1
            if "market" in txt.lower():
                market_index = ind + 1
            

        for player in  response.xpath("//table[@class='items']/tbody/tr"):

       
            player_name = player.xpath(f".//td[{player_indx}]/table//tr[1]//a/text()").get()
            player_link = player.xpath(f".//td[{player_indx}]/table//tr[1]//a/@href").get()
      
            position = player.xpath(f".//td[{player_indx}]/table//tr[2]/td/text()").get()
            Birth_date = player.xpath(f".//td[{Birth_index}]/text()").get()
            country = player.xpath(f".//td[{country_index}]//*[1]/@title").get()
            joined = player.xpath(f".//td[{join_index}]/text()").get()
            contract = player.xpath(f".//td[{contract_index}]/text()").get()
            marketValue = player.xpath(f".//td[{market_index}]/text()").get()
         


            try:
                player_name = player_name.replace("\n","").strip()
            except:
                pass            
            try:
                position = position.replace("\n","").strip()
            except:
                pass            

            league = response.meta['league']
            if "\n" in league:
                league = league.replace("\n","").strip()

            yield{
                'League':league,
                'Player Link':f"https://www.transfermarkt.co.uk{player_link}",
                'Player':player_name,
                'Position':position,
                'Date of Birth':Birth_date,
                'Country':country,
                'Join':joined,
                'Contract':contract,
                'Market Value':marketValue,
                'Club':response.meta['club'],
                'Club Link':response.meta['club_link'],
                'Season':response.meta['link']
            }