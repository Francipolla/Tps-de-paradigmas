import Personajes.*

object equipoPicante {
	
	const integrantes = #{toro,pamela}
	
	method integrantes() {
		return integrantes
	}
	method batallaContraEquipo(equipo){
		integrantes.forEach({integrante => integrante.enfrentarAVarios(equipo.integrantes())})
	}
	method batallaCampal(equipo) {
		self.batallaContraEquipo(equipo)
		equipo.batallaContraEquipo(self)
	}
	method energiaDeEquipo() {
		return integrantes.map({integrante => integrante.energia()}).sum()
	}	
	method gana(equipo){
		self.batallaCampal(equipo)
		return #{equipo,self}.max({team => team.energiaDeEquipo()}) 
	}	
	method ganador(equipo){
		return self.gana(equipo).integrantes().map({integrante => integrante.gritoDeVictoria()})
	}
	
}

object equipoHippie {
	
	const integrantes = #{tulipan,pocardo}
	
	method integrantes() {
		return integrantes
	}
	method batallaContraEquipo(equipo){
		integrantes.forEach({integrante => integrante.enfrentarAVarios(equipo.integrantes())})
	}
	method batallaCampal(equipo) {
		self.batallaContraEquipo(equipo)
		equipo.batallaContraEquipo(self)
	}
	method energiaDeEquipo() {
		return integrantes.map({integrante => integrante.energia()}).sum()
	}
	method gana(equipo){
		self.batallaCampal(equipo)
		return #{equipo,self}.max({team => team.energiaDeEquipo()}) 
	}	
	method ganador(equipo){
		return self.gana(equipo).integrantes().map({integrante => integrante.gritoDeVictoria()})
	}
	
}
