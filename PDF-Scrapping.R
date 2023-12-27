const fitz = require('fitz');
const re = require('re');
const pd = require('pandas');
const os = require('os');
const np = require('numpy');
const CarpetaEntrada = "INPUT";
const CarpetaDatos = "DATOS";
const CarpetaInformes = "INFORMES";
const CarpetaSalida = "OUTPUT";
const CarpetaResultados = "RESULTADOS";
const PathBase = os.getcwd();

function LeerFicherosPDF(ruta) {
  const ficheros = [];
  let subcarpetas = [];
  for (const [raiz, directorios, archivos] of os.walk(ruta)) {
    subcarpetas = directorios.sort((a, b) => parseInt(a.replace(/\D/g, '')) - parseInt(b.replace(/\D/g, '')));
  }
  for (const subcarpeta of subcarpetas) {
    const subcarpeta_ruta = os.path.normpath(os.path.join(raiz, subcarpeta));
  }
  for (const archivo of os.listdir(subcarpeta_ruta)) {
    if (archivo.endsWith('.pdf')) {
      ficheros.push(os.path.normpath(os.path.join(subcarpeta_ruta, archivo)));
    }
  }
  for (const archivo of os.listdir(ruta)) {
    if (archivo.endsWith('.pdf')) {
      ficheros.push(os.path.normpath(os.path.join(ruta, archivo)));
    }
  }
  return ficheros;
}

function LeerDocumento(nombreFichero) {
  const doc = fitz.open(nombreFichero);
  let text = "";
  for (const page of doc) {
    text = text + page.get_text();
  }
  return text.split('\n');
}

function BuscarValor(textoBuscar, lines) {
  const Encontrados = [];
  const valores = lines.map(line => re.search(textoBuscar, line) ? 1 : 0);
  const indices = valores.map((val, i) => val === 1 ? i : -1).filter(i => i !== -1);
  const posiciones = textoBuscar.length;
  for (const i of indices) {
    Encontrados.push(lines[i].slice(lines[i].lastIndexOf(textoBuscar) + posiciones).trim());
  }
  return Encontrados;
}

const fichero = os.path.normpath(os.path.join(PathBase, CarpetaEntrada, CarpetaDatos, "Diagnostico.xlsx"));
const diagnostico = pd.read_excel("INPUT/DATOS/Diagnostico.xlsx");
const diagnosticos_dic = Object.fromEntries(diagnostico["DIAGNÓSTICO"].map((diag, i) => [diag, diagnostico["NÚMERO DIAGNÓSTICO"][i]]));
for (const diagnostico in diagnosticos_dic) {
  const valor = diagnosticos_dic[diagnostico];
  console.log(diagnostico, valor);
}

const fichero = os.path.normpath(os.path.join(PathBase, CarpetaEntrada, CarpetaDatos, "Genes.xlsx"));
const genes = pd.read_excel(fichero);
const mutaciones = genes["GEN"].unique();
const mutaciones_dic = Object.fromEntries(genes["GEN"].map((gen, i) => [gen, genes["Número gen"][i]]));
for (const [gen, valor] of Object.entries(mutaciones_dic)) {
  console.log(gen, valor);
}

const rutaEntrada = os.path.normpath(os.path.join(PathBase, CarpetaEntrada, CarpetaInformes));
const fecha_Data = [];
const NHC_Data = [];
const Nbiopsia_Data = [];
const texto_Data = [];
const ficheros = LeerFicherosPDF(rutaEntrada);
for (const ficheroPDF of ficheros) {
  const lines = LeerDocumento(os.path.normpath(os.path.join(rutaEntrada, ficheroPDF)));
  NHC_Data.push(BuscarValor("NHC:", lines));
  Nbiopsia_Data.push(BuscarValor("biopsia:", lines));
  fecha_Data.push(BuscarValor("Fecha:", lines));
  texto_Data.push(BuscarValor("de la muestra:", lines));
}
console.log(NHC_Data);
console.log(Nbiopsia_Data);
console.log(fecha_Data);
console.log(texto_Data);

const textoDiag = [];
const numeroDiag = [];
for (const i of texto_Data) {
  const sinduplicados = [...new Set(i)];
  textoDiag.push(i.filter(x => sinduplicados.includes(x))[0]);
}
console.log(textoDiag);
for (const diagnostico of textoDiag) {
  const valor = diagnosticos_dic[diagnostico];
  numeroDiag.push(valor);
}
console.log(numeroDiag);

const NHC = [];
for (const i of NHC_Data) {
  const sinduplicadosNHC = [...new Set(i)];
  NHC.push(i.filter(x => sinduplicadosNHC.includes(x))[0]);
}
console.log(NHC);

const lista_resultante = [];
const elementos_vistos = new Set();
for (const sublist of Nbiopsia_Data) {
  const sublist_sin_duplicados = [];
  for (const elemento of sublist) {
    if (!elementos_vistos.has(elemento)) {
      sublist_sin_duplicados.push(elemento);
    }
    elementos_vistos.add(elemento);
  }
  lista_resultante.push(sublist_sin_duplicados);
}
console.log(lista_resultante);

const NB_values = lista_resultante.flat();
console.log(NB_values);

const biopsia = NB_values.map(x => x[2]);
const B = "1";
const C = "3";
const P = "2";
const Biopsia_solida = [];
for (const i of biopsia) {
  if (i === "B") {
    Biopsia_solida.push(B);
    console.log("1");
  } else if (i === "P") {
    Biopsia_solida.push(P);
    console.log("2");
  } else {
    Biopsia_solida.push(C);
    console.log("3");
  }
}
console.log(Biopsia_solida);

const fechas = [];
for (const i of fecha_Data) {
  const sinduplicados = [...new Set(i)];
  fechas.push(i.filter(x => sinduplicados.includes(x))[0]);
}
console.log(fechas);

const ficheros = LeerFicherosPDF(rutaEntrada);
const lista_ensayos = [];
const ensayos_finales = [];
for (const ficheroPDF of ficheros) {
  const lines = LeerDocumento(os.path.normpath(os.path.join(rutaEntrada, ficheroPDF)));
  let ensayos = 0;
  for (const line of lines) {
    const resultado = line.match(patron);
    if (resultado) {
      ensayos = parseInt(resultado[1]);
    }
  }
  lista_ensayos.push(ensayos);
}
console.log(lista_ensayos);
for (const i of lista_ensayos) {
  if (i === 0) {
    ensayos_finales.push(0);
  } else {
    ensayos_finales.push(1);
  }
}
console.log(ensayos_finales);

const ficheros = LeerFicherosPDF(rutaEntrada);
const lista_tratamientos = [];
const tratamientos_finales = [];
for (const ficheroPDF of ficheros) {
  const lines = LeerDocumento(os.path.normpath(os.path.join(rutaEntrada, ficheroPDF)));
  let tratamientos = 0;
  for (const line of lines) {
    const resultado = line.match(patron2);
    if (resultado) {
      tratamientos = parseInt(resultado[1]);
    }
  }
  lista_tratamientos.push(tratamientos);
}
console.log(lista_tratamientos);
for (const i of lista_tratamientos) {
  if (i === 0) {
    tratamientos_finales.push(0);
  } else {
    tratamientos_finales.push(1);
  }
}
console.log(tratamientos_finales);

for (const ficheroPDF of ficheros) {
  if (os.path.isfile(os.path.normpath(os.path.join(rutaEntrada, ficheroPDF))) && ficheroPDF.endsWith(".pdf")) {
    console.log(ficheroPDF);
  }
}

const numero_paciente = [];
for (const ficheroPDF of ficheros) {
  if (os.path.isfile(os.path.normpath(os.path.join(rutaEntrada, ficheroPDF))) && ficheroPDF.endsWith(".pdf")) {
    const [ruta1, fichero1] = os.path.split(ficheroPDF);
    const paciente = os.path.splitext(fichero1)[0];
    const pacientes = paciente[7];
    numero_paciente.push(pacientes);
  }
}
console.log(numero_paciente);

const chip2 = [];
for (const ficheroPDF of ficheros) {
  if (os.path.isfile(os.path.normpath(os.path.join(rutaEntrada, ficheroPDF))) && ficheroPDF.endsWith(".pdf")) {
    const resultado = ficheroPDF.match(patron);
    if (resultado) {
      const numero_chip = resultado[1];
      chip2.push(numero_chip);
    }
  }
}
console.log(chip2);

const ficheros = LeerFicherosPDF(rutaEntrada);
let max_mut = 0;
const genes_mut2 = {};
const frecuencias_totales = [];
const patron_frecuencia = /\d{2}\.\d{2}/;
for (const ficheroPDF of ficheros) {
  const nombreFichero = os.path.normpath(os.path.join(rutaEntrada, ficheroPDF));
  const lines = LeerDocumento(nombreFichero);
  let total_mut = 0;
  const encontrados2 = [];
  const lista_frec = [];
  for (const mutacion of mutaciones) {
    if (lines.includes(mutacion)) {
      const posicion = lines.indexOf(mutacion);
      if (mutacion === "FGFR4") {
        if (posicion < lines.length - 1 && lines[posicion + 1] === "p.(P136L)") {
          continue;
        }
      } else {
        let benigno = false;
        for (let a = posicion + 1; a < posicion + 10; a++) {
          if (lines[a].includes("Benign")) {
            benigno = true;
          }
        }
        if (!benigno) {
          total_mut += 1;
        }
        encontrados2.push(mutacion);
        console.log(nombreFichero + " - Existe: " + mutacion);
        if (!benigno) {
          for (const i of lines.slice(posicion, posicion + 10)) {
            const resultado = i.match(patron_frecuencia);
            if (resultado) {
              const frec = resultado[0];
              lista_frec.push(frec);
            }
          }
        }
      }
    }
  }
  genes_mut2[ficheroPDF.replace("\\", "_")] = encontrados2;
  if (total_mut > max_mut) {
    max_mut = total_mut;
  }
  frecuencias_totales.push(lista_frec);
}
console.log(frecuencias_totales);
const mut = Object.values(genes_mut2);
console.log(mut);
const num_mutaciones = mut.map(i => i.length);
console.log(num_mutaciones);
const numero_iden = mut.map(i => i.map(gen => mutaciones_dic[gen] || 0));
console.log(numero_iden);

const fusiones = [];
for (const ficheroPDF of ficheros) {
  const lines = LeerDocumento(os.path.normpath(os.path.join(rutaEntrada, ficheroPDF)));
  const variantes = [];
  for (const linea of lines) {
    for (const mutacion of mutaciones) {
      const patronGen = new RegExp("[A-Z0-9]{1,}-" + mutacion);
      // continue with the rest of the code
    }
  }
}


