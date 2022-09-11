
class Producto:
  def _init_( self, nombre, precio, cantidad ):
     self.nombre = nombre
     self.precio = precio
     self.cantidad = cantidad
  def verProducto( self ):
     print( "Producto: ", self.nombre, "\n   Precio: ", self.precio,  "\n   cantidad: " self.cantidad )

kumis = Producto( "Kumis", 2000, 30 )
kumis.verProducto()
