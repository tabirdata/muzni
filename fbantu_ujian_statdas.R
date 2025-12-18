fpaket <- function(paket1, ...) {
  nama_paket <- c(paket1, ...)
  for (k in seq_along(nama_paket)) {
    if (isFALSE(suppressWarnings(require(nama_paket[k], character.only = TRUE)))) {
      try({
        install.packages(nama_paket[k], repos = "https://cloud.r-project.org")
        library(nama_paket[k], character.only = TRUE)
      })
    }
  }
}
options(shiny.sanitize.errors = TRUE, OutDec = ",", max.print = 100)

fpaket(
  "shiny", "bslib", "rmarkdown"
)

kandidat <- reactive(readRDS(paste0(getwd(),"/basis_data_peserta/basis_statdas_2025.rds")))
soal_ujian_pertama <- readRDS("soal_ujian_pertama.rds")
jenis_soal_ujian_pertama <- readRDS("jenis_soal_ujian_pertama.rds")
pilihan_ganda_ujian_pertama <- readRDS("pilihan_ganda_ujian_pertama.rds")

NIM= ifelse(file.exists("nim_peserta.rds"), readRDS("nim_peserta.rds"), 13204195)
sensus = read.csv("census.csv")
sensus = sensus[, c("sex", "marital_status", "age", "race_general", "total_personal_income")]
burung = penguins[, c("species", "island", "bill_len", "sex", "body_mass")]
udara = airquality[, c("Ozone", "Solar.R", "Month", "Temp", "Wind")]
udara$Month = month.name[udara$Month]
titanic = as.data.frame(Titanic)
titanic$Class = as.character(titanic$Class)
titanic$Sex = as.character(titanic$Sex)
titanic$Age2= ifelse(titanic$Age == "Child", sample(3:10, 1), sample(20:60, 1))
titanic$Age = titanic$Age2
titanic$Age2 = NULL
gempa = quakes[, c("lat", "long", "depth", "mag")]
gempa$category = ifelse(gempa$mag < 5, "Level 1", "Level 2")
fbuat_data <- function(NIM) {
  NIM <- as.character(NIM)
  populasi_data <- list(burung, udara, gempa, titanic, sensus)
  benih <- as.numeric(paste(c(substr(NIM, 1, 2), substr(NIM, 11, 12)), collapse = ""))
  set.seed(benih)
  k <- sample(1:5, 1)
  dataku <- populasi_data[[k]]
  dataku <- dataku[sample(1:nrow(dataku), 300, replace = TRUE), ]
  rownames(dataku) <- NULL
  return(dataku)
}

datasaya <- fbuat_data(NIM)

fcek_waktu <- function(username) {
  readRDS(paste0("data_sisa_waktu/sisa_waktu_", username, ".rds"))
}
fberi_waktu <- function(username, nilai_menit) {
  saveRDS(nilai_menit, paste0("data_sisa_waktu/sisa_waktu_", username, ".rds"))
}
fbaca_jawaban <- function(username) {
  nama_folder <- paste0(getwd(), "/kumpulan_jawaban_peserta")
  daftar_file <- list.files(nama_folder)
  nama_file <- daftar_file[grep(pattern = username, x = daftar_file)][1]
  if (!is.na(nama_file)) {
    hasil <- readRDS(paste0(nama_folder, "/", nama_file))
  } else {
    hasil <- paste("Belum tersimpan jawaban untuk pengguna", username)
  }
  return(hasil)
}

fsoal <- function(kolom_soal, jenis_soal, isi_jawab, pilihan_ganda = NULL) {
  stopifnot(class(isi_jawab) == "list")
  semua_kartu <- vector("list", length(kolom_soal))
  nomor <- seq_along(kolom_soal)
  for (k in nomor) {
    semua_kartu[[k]] <- card(
      full_screen = TRUE,
      card_header(HTML(paste0("<h4>Soal-", nomor[k], "</h3>")),
                  class = "kotak_info"),
      HTML(kolom_soal[k]),
      height = ifelse(jenis_soal[k] == "isian",  "500px", "400px"),
      if (jenis_soal[k] == "isian") {
      textAreaInput(paste0("jawaban_", nomor[k]), h4("Jawab:"),
                    width = "1000px",
                    height = "200px",
                    value = isi_jawab[[k]]
      )} else if (jenis_soal[k] == "pilihan_ganda"){
        checkboxGroupInput(paste0("jawaban_", nomor[k]), h4("Jawab:"),
                    choices = pilihan_ganda[[k]], width = "1000px",
                    selected = isi_jawab[[k]]
                    )
      }
    )
  }
  semua_kartu
}

fsisa <- function(durasi_tes_menit, waktu_mulai) {
  stopifnot(
    "Data durasi_tes_menit harus berjenis numeric dan
            waktu_mulai dibuat dengan Sys.time()" =
      is.numeric(durasi_tes_menit) && inherits(waktu_mulai, "POSIXct")
  )
  sisa <- durasi_tes_menit - as.numeric(
    difftime(Sys.time(), waktu_mulai, units = "mins")
  )
  if (sisa < 0) {
    sisa <- 0
  }
  sisa_menit <- floor(sisa)
  sisa_detik <- round(60 * (sisa - sisa_menit))
  hasil <- list(
    sisa_angka = sisa, sisa_menit = sisa_menit, sisa_detik = sisa_detik
  )
  return(hasil)
}

fsimpan_jawab <- function(kelas_peserta, username, nama_peserta, isi_jawaban) {
  nama_file_jawaban <<- paste0(
    getwd(), "/kumpulan_jawaban_peserta/", kelas_peserta, "_",
    username, "_", nama_peserta, ".rds"
  )
  saveRDS(isi_jawaban, nama_file_jawaban)
}


fgabung_pipa_plus = function(teks_kode){
  terpilah1 = strsplit(teks_kode, "\n") |>
    unlist() |> trimws()
  pos_pipa_akhir = which(grepl("\\|>$|\\+$", x = terpilah1))
  if (length(pos_pipa_akhir) < 1) {
    return(terpilah1)
  } else {
  tidak_urut = abs(diff(pos_pipa_akhir)) > 1
  indeks_pilah = c(0, cumsum(tidak_urut))
  pos_pipa_gabung =  split(pos_pipa_akhir, indeks_pilah)
  gabung_pipa = vector("list", length(pos_pipa_gabung))
  for(k in seq_along(pos_pipa_gabung)){
    pos_pipa_gabung[[k]] = c(pos_pipa_gabung[[k]], max(pos_pipa_gabung[[k]]) + 1)
    gabung_pipa[[k]] = paste(terpilah1[pos_pipa_gabung[[k]]], collapse = " ")
  }
  pos_lain = setdiff(1:length(terpilah1), unlist(pos_pipa_gabung))
  gabung_semua = c(list(terpilah1[pos_lain]), gabung_pipa)
  return(unlist(gabung_semua))
  }
}


fsaring = function(teks_kode){
  saring = function(teks_kode_satu){
    teks_kode_satu = as.character(teks_kode_satu)
    if(grepl("rm|ls|list.files|get|Sys.getenv|Sys.setenv|options", x = teks_kode_satu)){
      teks_kode_satu =
        gsub("rm\\(|ls\\(|list.files\\(|get\\(|Sys.getenv|Sys.setenv|options", "#", x = as.character(teks_kode_satu)) |>
         gsub("View", "head", x = _) |>
          gsub("\\+", "\\+\n", x = _)
    } else  teks_kode_satu
  }
  hasil = lapply(teks_kode, saring) |> unlist()
  return(hasil)
}

fpilah = function(teks_kode){
 teks_kode = teks_kode |> fgabung_pipa_plus() |> fsaring()
 daftar_parsed_token = vector("list", length(teks_kode))
 daftar_parsed_text = vector("list", length(teks_kode))
 for(k in seq_along(teks_kode)){
   daftar_parsed_token[[k]] = getParseData(parse(text = teks_kode[k]))$token
   daftar_parsed_text[[k]] = getParseData(parse(text = teks_kode[k]))$text
 }

 pos_gambar =
   grepl("plot|ggplot|geom_|pairs|boxplot|hist|histogram|dotplot|curve|pie|dotchart|lines|points|text|abline|rect|polygon",
         x = daftar_parsed_text) &
   grepl("SYMBOL_FUNCTION_CALL",
         x = daftar_parsed_token)
 hasil_gambar = teks_kode[pos_gambar]
 hasil_teks = setdiff(teks_kode, hasil_gambar)
 return(list(kode_gambar = hasil_gambar, kode_teks = hasil_teks))
}

fjalankan_teks = function(teks_kode){
  tersaring = fpilah(teks_kode)
  tryCatch(
    for(k in seq_along(tersaring$kode_teks)){
      print(eval(parse(text = tersaring$kode_teks[k])))
    },
    error = function(e) message(paste("Terjadi kesalahan:", e))
  )
}

fjalankan_gambar = function(teks_kode){
  tersaring = fpilah(teks_kode)
  tryCatch(
    for(k in seq_along(tersaring$kode_gambar)){
      print(eval(parse(text = tersaring$kode_gambar[k])))
    },
    error = function(e) message(paste("Terjadi kesalahan:", e))
  )
}

fsandi = function(nama_file_input, kunci, nama_file_output){
  if (file.exists(nama_file_output)){
    file.remove(nama_file_output)
  }
  encrypt_file(nama_file_input, key = kunci,  outfile = nama_file_output)
}

fbuka_sandi = function(nama_file, kunci){
  decrypt_file(nama_file, key = kunci, outfile = gsub(".statdas", "", x = nama_file))
}


pengguna = readRDS(paste0(getwd(),"/basis_data_peserta/basis_statdas_2025.rds"))
gabung_perbaris = vector("list", nrow(pengguna))
for(k in 1:nrow(pengguna)){
 gabung_perbaris[[k]] = paste(c("    - name:", "      password:", "      group:"),
                              c(pengguna[k, c("username", "kunci")], "mahasiswa"))
}
do.call(cbind, gabung_perbaris) |> writeLines(con = "coba.txt")
