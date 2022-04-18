library(shiny)
library(plotly)

# Pada part ini, kita akan focus ke pembuatan layout
# Theme yang kita pake adalah https://demos.creative-tim.com/marketplace/black-dashboard-pro/examples/dashboard.html
# Layouting mungkin berbeda untuk theme yang berbeda
# Be creative...

# 4. Buat function untuk generate card wdget
topWidget <- function(icon, category, title, iconFooter, labelFooter) {
  div(
    class = "card card-stats",
    div(
      class = "card-body",
      div(
        class = "row",
        div(
          class = "col-5",     # for icon
          div(
            class = "info-icon text-center icon-warning",  # info-icon=container icon, text-center=centering the content, icon-warning=memberikan warna warning (primary, success, danger, warning) --> https://www.w3schools.com/bootstrap4/bootstrap_colors.asp. 
            span(class = paste("las", icon))  # icon taken from lineawesome, pass as parameter 
          )
        ),
        div(
          class = "col-7",  # for content
          div(
            class = "numbers",
            p(class = "card-category", category), # category pass as argument
            h3(class = "card-title", title) # title berisi angka indikator.  pass as argument
          )
        )
      )
    )
    # div(
    #   class = "card-footer",
    #   hr(),
    #   div(
    #     class = "stats",
    #     span(class = paste("las", iconFooter)),
    #     labelFooter
    #   )
    # )
  )
}

bottomWidget <- function(content) {
  div(
    class = "card card-stats",
    div(
      class = "card-body",
      content
    )
  )
}

ui <- div(
  class = "wrapper",
  # 1. Tambahkan css dari theme yang kita pilih pada top level element UI
  tags$style(HTML("
    @import url('https://fonts.googleapis.com/css?family=Poppins');
    @import url('https://demos.creative-tim.com/marketplace/black-dashboard-pro/assets/css/black-dashboard.min.css?v=1.1.1');
    @import url('https://maxst.icons8.com/vue-static/landings/line-awesome/line-awesome/1.3.0/css/line-awesome.min.css');

    .navbar {
      top: auto;
    }
    .card-stats .info-icon span {
      color: #fff;
      font-size: 1.7em;
      padding: 14px 13px;
    }
    .card-chart .chart-area {
      height: 300px;
    }
    .plotly {
      height: auto !important;
    }
  ")),
  div(
    class = "main-panel",
    # 2. Buat judul di navbar
    div(
      class = "navbar justify-content-center",
      div(class = "navbar-brand", "DASHBOARD")
    ),
    # 3. Buat layout untuk content
    div(
      class = "p-4",
      div(
        class = "row",
        # 5. Buat 4 buah widget dengan ukuran 1/4 lebar halaman
        # Gunakan textOutput sebagai placeholder output
        div(
          class = "col-lg-3 col-md-6", 
          topWidget(icon = "la-syringe", category = "Country with The Highest GDP", title = textOutput("max_gdp_per_capita"), iconFooter = "la-random", labelFooter ="Last Update xxxx")
        ),
        div(
          class = "col-lg-3 col-md-6", 
          topWidget(icon = "la-syringe", category = "Country with The Highest Healthy Life Expectancy", title = textOutput("max_healthy_life_expectancy"), iconFooter = "la-random", labelFooter ="Last Update xxxx")
        ),
        div(
          class = "col-lg-3 col-md-6", 
          topWidget(icon = "la-syringe", category = "Country with The Highest Social Support Index", title = textOutput("max_social_support"), iconFooter = "la-random", labelFooter ="Last Update xxxx")
        ),
        div(
          class = "col-lg-3 col-md-6", 
          topWidget(icon = "la-syringe", category = "Country with The Highest Freedom to Make Life Choices", title = textOutput("max_freedom"), iconFooter = "la-random", labelFooter ="Last Update xxxx")
        ),
        # 6. Buat 1 buah widget dengan ukuran full untuk chart
        # Gunakan plotlyOutput sebagai placeholder map
        div(
          class = "col-12",   # center widget, dibagi 2: header dan body
          div(
            class = "card card-chart",  
            div(
              class = "card-header",  #header, dibagi 2: title dan slider
              div(
                class = "row",
                div(
                  class = "col-sm-6 text-left",
                  h5(class = "card-category", "Overall"),
                  h2(class = "card-title", span(class = "las la-bell text-primary"), "Rank")
                ),
                div(
                  class = "col-sm-6",
                  uiOutput("slider_n")
                )
              )
            ),
            div(
              class = "card-body",
              div(
                class = "chart-area",
                plotlyOutput("peta_happiness")
              )
            )
          )
        ),
        div(
          class = "col-lg-6 col-md-12", 
          bottomWidget(content = plotlyOutput("bar_happiness"))
        ),
        div(
          class = "col-lg-6 col-md-12", 
          bottomWidget(content = 
                         h2("Summary: Kebahagian suatu negara ditentukan oleh tingkat ekonomi suatu negara dibuktikan dengan rata-rata negara maju memiliki ranking kebahagiaan yang lebih tinggi daripada negara-negara berkembang")
          )
        )
      )
    )
  )
)