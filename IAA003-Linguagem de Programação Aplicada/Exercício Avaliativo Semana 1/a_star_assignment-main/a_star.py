import dists

class AStar:
    
    """
    Feito por: Clístenes Grizafis Bento
    Professor: Prof. Alexander Robert Kutzke
    """
    
    def __init__(self):
        """
        Construtor de classe
        """       
        print("Para ver o melhor caminho para Bucharest de determinada cidade,")
        print("use o comando:")
        print(" a_star.get_best_path_string('NOME_DA_CIDADE')")
        
    def add_city_on_border(self, tup_city):
        """
        Adiciona cidade na borda
        """
        city,distance = tup_city 
        validation = True
        for visited_city in self.explored:           
            if visited_city == city:
                validation = False
        #print(self.border)
        for border_city in self.border:
           name_boder_city = border_city.get('cidade') 
           if(name_boder_city == city):
                c = (self.walked_distance + distance)+self.straight_line_list[city]
                if (border_city.get('custo')< c):
                    validation = False
        if validation:            
            cidade = city            
            origem = self.node.cidade
            custo = (self.walked_distance + distance)+self.straight_line_list[city]                
            self.border.append({"cidade": cidade, "origem": origem, "custo": custo})
   
    def remove_city_on_border(self, city):
        """
        remove cidade da borda
        """
        for borders_city in self.border:
            if city == borders_city.get('cidade'):
                self.border.remove(borders_city)    

    def move_to_next_city(self):
        """
        muda o valor do nó para os valores da cidade vizinha com menor custo
        """
        coast = self.border[0].get('custo')
        city = self.border[0].get('cidade')
        origin = self.border[0].get('origem') 
        for border_city in self.border:
             if border_city.get('custo')<coast:
                 coast = border_city.get('custo')
                 city = border_city.get('cidade')
                 origin = border_city.get('origem')
        
        for connection in self.connections_list[ self.node.cidade]:
            con_city, distance = connection
            if con_city == city:
                self.walked_distance = self.walked_distance + distance
            
        self.node.cidade = city
        self.node.origem = origin
        self.node.conexoes = self.connections_list[city]
        self.node.dist_linha_reta = self.straight_line_list[city]        
        self.great_path.append(self.node.origem)
        self.great_path = list(dict.fromkeys(self.great_path))
                   
    def search_best_path(self, start, goal='Bucharest'):
        """
        Retorna uma lista com o caminho de start até 
        goal segundo o algoritmo A*
        """
        
        self.straight_line_list = dists.straight_line_dists_from_bucharest
        self.connections_list = dists.dists        
        self.great_path = []
        self.explored = []
        self.walked_distance = 0
        self.node = Node(start, self.straight_line_list[start], '', self.connections_list[start])  
        self.border =[{"cidade": start, "origem": '', "custo": self.straight_line_list[start]},]
        self.great_path.append(self.node.cidade)
        if(self.node.cidade == 'Bucharest'):
            return (self.great_path)
        else:
            while (self.node.cidade != 'Bucharest'):            
                self.explored.append(self.node.cidade)               
                self.remove_city_on_border(self.node.cidade)                
                for city in self.node.conexoes:
                    #print(city)
                    
                    self.add_city_on_border(city)
                self.move_to_next_city()
               
            self.great_path.append(self.node.cidade)
            return(self.great_path)
    def get_best_path_string(self, start):
        """
        Retorna na tela uma lista com o caminho de start até 
        goal segundo o algoritmo A*
        """
        print("O melhor caminho encontra-se na sequencia de cidades:")
        print(self.search_best_path(start))            
                   
class Node:
    def __init__(self, cidade, dist_linha_reta, origem, conexoes):
        self.cidade = cidade
        self.dist_linha_reta = dist_linha_reta
        self.origem = origem
        self.conexoes = conexoes

if __name__ == '__main__':   
    a_star = AStar()

    
    