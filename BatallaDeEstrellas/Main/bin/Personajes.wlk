object pamela {
	
	var botiquin = ["algodon","aguaOxigenada","cintaDePapel","cintaDePapel"]
	var energia = 6000
	const gritoDeVictoria = "AcaPasoPamela"
	
	method agregarObjetos(objeto) {
		botiquin = botiquin+objeto
	}
	method objetos() {
		return botiquin
	}
	method energia() {
		return energia
	}
	method gritoDeVictoria() {
		return gritoDeVictoria
	}
	method aumentarEnergia (masEnergia) {
		 energia = energia + masEnergia
	}
	method disminuirEnergia (menosEnergia) {
		 energia = (energia - menosEnergia).max(0)
	}
	method pelearManoAMano(enemigo) {
		 self.aumentarEnergia(400)
		 enemigo.efectoEnemigo(0)
	}
	method efectoEnemigo(efecto) {
	     self.disminuirEnergia(efecto)
	}
	method enfrentarAVarios(enemigos) {
		enemigos.forEach({enemigo => self.pelearManoAMano(enemigo)})
	}
}

object pocardo {
	
	var botiquin = ["guitarra","curitas","cotonetes"]
	var energia = 5600
	const gritoDeVictoria = "SienteElPoderDeLaMusica"
	
	method agregarObjetos(objeto) {
		botiquin = botiquin+objeto
	}
	method objetos() {
		return botiquin
	}
	method energia() {
		return energia
	}
	method gritoDeVictoria() {
		return gritoDeVictoria
	}
	method aumentarEnergia (masEnergia) {
		 energia = energia + masEnergia
	}
	method disminuirEnergia (menosEnergia) {
		 energia = (energia - menosEnergia).max(0)
	}
	method pelearManoAMano(enemigo) {
		 self.aumentarEnergia(500)
		 enemigo.efectoEnemigo(0)
	}
	method efectoEnemigo(efecto) {
	     self.disminuirEnergia(efecto)
	}
	method enfrentarAVarios(enemigos) {
		enemigos.forEach({enemigo => self.pelearManoAMano(enemigo)})
	}
}

object tulipan {
	
	var energia = 8400
	const gritoDeVictoria = "HoraDeCuidarLasPlantas"
	var galpon = ["rastrillo","maceta","maceta","manguera"]
	
	method agregarObjetos(objeto) {
		galpon = galpon+objeto
	}
	method objetos() {
		return galpon
	}
	method energia() {
		return energia
	}
	method gritoDeVictoria() {
		return gritoDeVictoria
	}
	method aumentarEnergia (masEnergia) {
		energia = energia + masEnergia
	}
	method disminuirEnergia (menosEnergia) {
	    energia = (energia - menosEnergia).max(0)
	}
	method pelearManoAMano(enemigo) {
		 enemigo.efectoEnemigo(enemigo.energia()*0.5)
	}
	method efectoEnemigo(efecto) {
	     self.disminuirEnergia(efecto)
	}
	method enfrentarAVarios(enemigos) {
		enemigos.forEach({enemigo => self.pelearManoAMano(enemigo)})
	}
}

object toro {
	
	const botin = [].asSet()
	var energia = 7800
	const gritoDeVictoria = "NoSeMetanConElToro"
	
	method agregarUltimoElementoABotin(coleccion) {
		botin.add(coleccion.last())
	}
	method energia() {
		return energia
	}
	method gritoDeVictoria() {
		return gritoDeVictoria
	}
	method aumentarEnergia (masEnergia) {
		 energia = energia + masEnergia
	}
	method disminuirEnergia (menosEnergia) {
		 energia = (energia - menosEnergia).max(0)
	}
	method pelearManoAMano(enemigo) {
		 enemigo.efectoEnemigo(enemigo.objetos().size()*200)
		 self.agregarUltimoElementoABotin(enemigo.objetos())
	}
	method efectoEnemigo(efecto) {
	      self.disminuirEnergia(efecto)
	}
	method enfrentarAVarios(enemigos) {
		enemigos.forEach({enemigo => self.pelearManoAMano(enemigo)})
	}
	
}