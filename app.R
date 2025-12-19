source("fbantu_ujian_statdas.R")
library(shiny)
library(bslib)
library(rmarkdown)
ui <- page_navbar(
  fillable = TRUE, fillable_mobile = TRUE,
  window_title = "Ujian Statistika Dasar",
  navset_hidden(
    id = "laman_utama",
    nav_panel_hidden(
      value = "laman_identitas",
      layout_columns(
        col_widths = 3,
        card(
          min_height = 200, fill = FALSE,
          card_header("Pemeriksaan Identitas", class = "judul_kartu"),
          textInput("pengguna", "Nama Pengguna Anda:", width = '250px'),
          passwordInput("kunci", "Kata Kunci:", width = '250px'),
          actionButton("tombol_masuk", "Masuk", class = "btn-info", width = '193px')
        )
      )
    ),
    nav_panel(
      value = "laman_ketentuan",
      HTML("<h4 style = 'text-align: center'>
        Ujian Pertama Mata Kuliah TKL1203 Statistika Dasar<br>
         Program Studi Teknik Lingkungan Fakultas Teknik Universitas Jember <br>
         Semester Genap 2025/2026</h4><h5>Desember 2025</h5>"),
      width = 8,
      layout_column_wrap(
        card(
          card_header("Petunjuk dan Ketentuan Teknis Ujian Statistika Dasar",
                      class = "judul_kartu"
          ),
          HTML(readLines(paste("www/ketentuan.html", collapse = "\n"))),
          checkboxInput("setuju",
                        width = "800px",
                        label = "Saya menyatakan setuju dan patuh terhadap semua ketentuan tersebut.",
                        value = FALSE
          ),
          actionButton("tombol_mulai", "Mulai Ujian",
                       width = "200px",
                       class = "btn-primary", disabled = TRUE
          )
        )
      )
    ),
    nav_panel_hidden(
      value = "laman_soal",
      layout_sidebar(
        sidebar = sidebar(
          width = 425,
          row_heights = "200px", fill = TRUE,
          card(
            min_height = 300, fill = TRUE, full_screen = TRUE,
            card_header("Identitas Anda", class = "kotak_info"),
            htmlOutput("identitas")
          ),
          card(
            min_height = 250,
            fill = TRUE, full_screen = TRUE,
            card_header("Sisa Waktu Anda", class = "kotak_info"),
            htmlOutput("timer")
          ),
          card(
            min_height = 200, fill = TRUE, full_screen = TRUE,
            card_header("Status Jawaban Anda", class = "kotak_info"),
            textOutput("terjawab")
          ),
          card(
            min_height = 200, fill = TRUE, full_screen = TRUE,
            card_header("Simpan Jawaban Anda", class = "kotak_info"),
            layout_columns(
              tags$div(actionButton("tombol_ok_simpan", HTML("Simpan"),
                                    width = "120px",
                                    class = "btn-success"
              )),
              tags$div(actionButton("tombol_ok_selesai", "Selesai",
                                    width = "120px",
                                    class = "btn-warning"
              ))
            )
          )
        ),
        nav_panel_hidden(
          value = "laman_soal",
          layout_sidebar(
            fillable = TRUE,
            sidebar = sidebar(
              position = "right", width = 800, open = FALSE,
              h5("Perhatian: Ini BUKAN LEMBAR JAWABAN. Ini hanya tempat uji coba kode R. Untuk menulis jawaban, ketik kembali kode yang sudah Anda uji coba di sini ke laman jawaban pada soal."),
              textAreaInput("kode", h3("Tempat menulis kode R:"),
                            height = 400
              ),
              actionButton("jalankan", "Jalankan (Run)",
                           width = "200px",
                           class = "btn-success"
              ),
              br(),
              card(
                min_height = 300, full_screen = TRUE, fill = TRUE,
                card_header("Tempat Hasil Teks (Console)", class = "kotak_info"),
                verbatimTextOutput("hasil_kode_teks")
              ),
              br(),
              card(
                full_screen = TRUE, min_height = 200, fill = TRUE,
                card_header("Tempat unduh datasaya", class = "kotak_info"),
                uiOutput("unduh_datasaya")
              )
            ),
            uiOutput("soal_soal", fill = TRUE)
          )
        )
      )
    ),
    nav_panel_hidden(
      value = "laman_tutup",
      layout_columns(
        col_widths = 4,
        card(
          height = "300px",
          card_header("Terima Kasih!", class = "kotak_info"),
          HTML("<h5>Terima kasih atas partisipasi Anda dalam ujian ini.
    Jawaban Anda sudah terkirim ke pengampu. Anda dapat mengunduh salinan jawaban Anda lalu menutup laman ini.</h5>"),
          downloadButton("unduh_jawaban", "Unduh File",
                         width = "200px",
                         class = "btn-success"
          )
        )
      )
    )),
  tags$script(HTML("
  $('body').bind('cut copy paste', function (e) {
    e.preventDefault();});
  $('body').bind('contextmenu', function(e){
  return false;
  })")),
  tags$style(
    ".judul_kartu {
          text-align: center;
          background-color: #007bc2;
          color: #FCFCFC;
        }
      .kotak_info{
          text-align: center;
          font-size = '30px';
          color: #FCFCFC;
          background-color: #5BC2BC;
      }
      .waktu {
        font-size:35px;
        color:darkred;
        opacity: inherit !important;
      }
    "
  )
)


server <- function(input, output, session) {
  observe({
    nav_show(id = "laman_utama", target = "laman_identitas")
    nav_hide(id = "laman_utama", target = "laman_ketentuan")
  })
  username <<- reactive(trimws(as.character(input$pengguna)))

  kunci <- reactive(trimws(as.character(input$kunci)))
  observeEvent(input$tombol_masuk, {
    req(username())
    req(kunci())
    if (!username() %in% kandidat()$username ||
        !kunci() %in% kandidat()$kunci) {
      showNotification("Nama pengguna atau kata kunci salah.",
                       duration = 2,
                       type = "error"
      )
    } else {
      showNotification("Login berhasil. Laman ketentuan akan dibuka...",
                       type = "message", duration = 2
      )
      Sys.sleep(1)
      nav_show("laman_utama", target = "laman_ketentuan", select = TRUE)
      nav_hide("laman_utama", target = "laman_identitas")
    }
  })
  observe({
    if (isTRUE(input$setuju)) {
      updateActionButton(
        inputId = "tombol_mulai",
        label = "Mulai Ujian",
        disabled = FALSE
      )
    }
  })


  observeEvent(input$tombol_mulai,
               {
                 nama_kandidat <<- reactive(kandidat()$nama[kandidat()$username ==
                                                              trimws(username())])
                 kelas_kandidat <<- reactive(kandidat()$kelas[kandidat()$username ==
                                                                trimws(username())])
                 nim_kandidat <<- reactive(kandidat()$nim[kandidat()$username ==
                                                            trimws(username())])
                 durasi_ujian <<- 70.05
                 observe({
                   if (file.exists(paste0(
                     "data_sisa_waktu/sisa_waktu_",
                     username(), ".rds"
                   ))) {
                     sisa_waktu_saat_login <<- reactive(
                       readRDS(paste0(
                         "data_sisa_waktu/sisa_waktu_",
                         username(), ".rds"
                       ))
                     )
                   } else {
                     sisa_waktu_saat_login <<- reactive(durasi_ujian)
                   }
                   if (sisa_waktu_saat_login() < 0.01) {
                     nav_remove(id = "laman_utama", target = "laman_ketentuan")
                     nav_remove(id = "laman_utama", target = "laman_soal")
                     nav_show(id = "laman_utama", target = "laman_tutup", select = TRUE)
                   }
                 })
                 menit_mulai <- Sys.time()
                 output$timer <- renderUI({
                   invalidateLater(1000)
                   sisa_setelah_mulai <<- fsisa(
                     durasi_tes_menit = sisa_waktu_saat_login(),
                     waktu_mulai = menit_mulai
                   ) |> reactive()
                   saveRDS(
                     sisa_setelah_mulai()$sisa_angka,
                     paste0("data_sisa_waktu/sisa_waktu_", username(), ".rds")
                   )
                   saveRDS(
                     username(),
                     paste0("username_kandidat_", username(), ".rds")
                   )
                   saveRDS(
                     nama_kandidat(),
                     paste0("nama_kandidat_", username(), ".rds")
                   )
                   saveRDS(
                     kelas_kandidat(),
                     paste0("kelas_kandidat_", username(), ".rds")
                   )
                   observe({
                     if (sisa_setelah_mulai()$sisa_angka < 0.01) {
                       nav_remove(id = "laman_utama", target = "laman_ketentuan")
                       nav_remove(id = "laman_utama", target = "laman_soal")
                       nav_show(id = "laman_utama", target = "laman_tutup", select = TRUE)
                     }
                   })
                   HTML(
                     "<h3 class = 'waktu'>", sisa_setelah_mulai()$sisa_menit, "menit",
                     sisa_setelah_mulai()$sisa_detik, "detik </h3>"
                   )
                 })
                 output$identitas <- renderUI({
                   div(
                     HTML(
                       "<h4 style = 'color:#27699C;'><b>Nama:</b><br></h4><h5>",
                       nama_kandidat(), "</h5>"
                     ),
                     HTML(
                       "<h4 style = 'color:#27699C;'><b>NIM:</b><br></h4><h5>",
                       nim_kandidat(), "</h5>"
                     ),
                     HTML(
                       "<h4 style = 'color:#27699C;'><b>Kelas:</b><br><h5>",
                       kelas_kandidat(), "</h5>"
                     )
                   )
                 })
                 dataku <<- reactive(fbuat_data(nim_kandidat()))
                 saveRDS(nim_kandidat(), "nim_kandidat.rds")
                 jawaban_tersimpan <<- if (file.exists(
                   paste0(
                     getwd(), "/kumpulan_jawaban_peserta/", kelas_kandidat(), "_",
                     username(), "_", nama_kandidat(), ".rds"
                   )
                 )) {
                   reactiveFileReader(1000, session,
                                      filePath = paste0(
                                        getwd(), "/kumpulan_jawaban_peserta/", kelas_kandidat(), "_",
                                        username(), "_", nama_kandidat(), ".rds"
                                      ),
                                      readFunc = readRDS
                   )
                 } else {
                   daftar_jawaban <- vector("list", length(soal_ujian_pertama))
                   names(daftar_jawaban) <- paste0("jawab_", 1:length(soal_ujian_pertama))
                   for (k in seq_along(daftar_jawaban)) {
                     daftar_jawaban[[k]] <- ""
                   }
                   reactive(daftar_jawaban)
                 }
                 output$soal_soal <- renderUI({
                   req(kelas_kandidat())
                   nav_panel(
                     title = "Soal-Soal Ujian",
                     fsoal(
                       kolom_soal = soal_ujian_pertama, jenis_soal = jenis_soal_ujian_pertama,
                       isi_jawab = jawaban_tersimpan(),
                       pilihan_ganda = pilihan_ganda_ujian_pertama
                     )
                   )
                 })
               },
               once = TRUE
  )
  observeEvent(input$tombol_mulai, {
    nav_hide(id = "laman_utama", target = "laman_ketentuan")
    nav_show(id = "laman_utama", target = "laman_soal", select = TRUE)
    file.copy(
      paste0(getwd(), "/salinan_jawaban_kandidat.Rmd"),
      paste0(getwd(), "/salinan_jawaban_kandidat_", username(), ".Rmd")
    )

    jawaban <<- reactive({
      daftar_jawaban <- vector("list", length(soal_ujian_pertama))
      names(daftar_jawaban) <- paste0("jawab_", 1:length(soal_ujian_pertama))
      for (k in seq_along(daftar_jawaban)) {
        daftar_jawaban[[k]] <- ""
      }
      daftar_jawaban
    })


    kosong <<- reactive(which(sapply(jawaban(), function(x) sum(nchar(x))) < 1))
    output$terjawab <- renderText({
      if (length(kosong()) < 1) {
        paste("Semua soal sudah terisi jawaban.")
      } else {
        paste(paste0("Soal-", kosong(), collapse = ", "), "masih kosong.
          Silakan isi dengan jawaban.")
      }
    })
    observe({
      jawaban <<- reactive(list(
        input$jawaban_1, input$jawaban_2, input$jawaban_3,
        input$jawaban_4, input$jawaban_5, input$jawaban_6
      ))
      if (length(kosong()) < 1) {
        updateActionButton(session,
                           inputId = "tombol_ok_selesai",
                           label = "Selesai",
                           disabled = FALSE
        )
      }
    })

    observe({
      output$hasil_kode_teks <- renderPrint({
        req(input$kode)
        isi_kode_teks = reactive(input$kode)
        fjalankan_teks(isi_kode_teks())
      }) |> bindEvent(input$jalankan)
    


        # ganti dengan unduh

        output$unduh_datasaya <- renderUI(
          downloadButton("tombol_unduh_datasaya", "unduh datasaya", class = 'btn-success')
        )
        output$tombol_unduh_datasaya <- downloadHandler(
          filename = function() {"datasaya.RData"},
          content = function(file){
            save(datasaya, file = file)
          }
        )

        # akhir bagian unduh

      })

      observe({
        render(
          input = "salinan_jawaban_kandidat.Rmd",
          output_file = paste0(
            "salinan_jawaban_", kelas_kandidat(), "_",
            nama_kandidat(), ".html"
          ),
          envir = globalenv()
        )
      }) |> bindEvent(input$tombol_ok_simpan, input$tombol_ok_selesai)
    })
    # awalnya di observeEvent(input$tombol_mulai)
    observe({
      fsimpan_jawab(kelas_kandidat(), username(), nama_kandidat(), jawaban())
      showNotification("Jawaban Anda sudah tersimpan.", duration = 2)
    }) |> bindEvent(input$tombol_ok_simpan)


    observe({
      fsimpan_jawab(kelas_kandidat(), username(), nama_kandidat(), jawaban())
      showNotification("Jawaban Anda sudah tersimpan.
                     Laman ini akan ditutup.", duration = 2)
      Sys.sleep(1)
      nav_remove(id = "laman_utama", target = "laman_soal")
      nav_show(id = "laman_utama", target = "laman_tutup", select = TRUE)
    }) |> bindEvent(input$tombol_ok_selesai)


    output$unduh_jawaban <- downloadHandler(
      filename = function() {
        paste0(
          "Salinan Jawaban ",
          nama_kandidat(), ".zip"
        )
      },
      content = function(file) {
        render(
          input = paste0(getwd(), "/salinan_jawaban_kandidat_", username(), ".Rmd"),
          output_file = paste0("Salinan Jawaban ", nama_kandidat(), ".html"),
          envir = globalenv()
        )
        zip(file,
            files = c(paste0("Salinan Jawaban ", nama_kandidat(), ".html"),
                      paste0(
                        getwd(), "/kumpulan_jawaban_peserta/", kelas_kandidat(), "_",
                        username(), "_", nama_kandidat(), ".rds")),
            flags = paste("--password", "mudamulia")
        )
      }
    )
  }

  shinyApp(
    ui = ui,
    server = server
  )








