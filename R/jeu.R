#' Launch the Shiny app
#'
#' @export
lance <- function() {
  library(shiny)

  # Fonction pour générer une grille vide
  generateEmptyGrid <- function(gridSize) {
    matrix(0, nrow = as.numeric(gridSize), ncol = as.numeric(gridSize))
  }

  # Fonction pour générer une grille aléatoire
  generateRandomGrid <- function(gridSize) {
    matrix(
      sample(c(1,1,0), as.numeric(gridSize)^2, replace = TRUE),
      nrow = as.numeric(gridSize),
      ncol = as.numeric(gridSize)
    )
  }


  # Fonction pour compter les groupes dans une ligne
  countGroups <- function(row) {
    rle_values <- rle(row)
    count_groups <- sum(rle_values$values == 1)
    count_cells_in_groups <- rle_values$lengths[rle_values$values == 1]
    return(list(count_groups = count_groups, count_cells_in_groups = count_cells_in_groups))
  }


  # Interface utilisateur Shiny
  ui <- fluidPage(
    titlePanel("🎮 Jeux de Picross"),
    sidebarLayout(
      sidebarPanel(
        selectInput("gridSize", "Choisissez la taille de la grille", choices = 3:10, selected = 6),
        actionButton("revealSolution", "Révéler/Cachez la solution", class = "my-button"),
        div(
          class = "difficulty-buttons-container",
          actionButton("easyMode", "Facile", class = "my-button"),
          actionButton("NormalMode", "Normal", class = "my-button"),
          actionButton("hardMode", "Difficile", class = "my-button")
        )
      ),
      mainPanel(
        fluidRow(
          div(
            style = "display: flex; justify-content: flex-start; margin-left: 60px;",
            uiOutput("grid1RowNumbers")
          )
        ),
        fluidRow(
          div(
            style = "margin-left: 60px;",
            uiOutput("gridOutput1")
          ),
          div(
            style = "margin-left: 30px;",
            uiOutput("gridOutput2")
          )
        ),
        fluidRow(
          div(
            style = "margin-top: 20px; margin-left: 60px;",
            textOutput("difficultyLevel")
          )
        )
      )
    ),
    tabPanel("Règles", fluidPage(
      titlePanel("Règles du Jeu"),
      fluidRow(
        actionButton("toggleRules", "Afficher/Cacher les règles", class = "my-button"),
        div(
          id = "rulesContainer",
          class = "container",
          style = "border: 2px solid #ccc; padding: 20px; border-radius: 10px; margin-left: 0;",
          column(12, HTML("<b>Il s'agit d'un jeu de picross. Le but est de trouver la grille cachée. Cette grille est générée de manière aléatoire. Pour vous aider, il y a des indices à côté des lignes et en haut des colonnes. L'indice n-m signifie que dans la ligne ou la colonne, il y a n+m cases coloriées à trouver, et que n des cases sont adjacentes, puis séparées d'au moins une case vide, il y a m cases adjacentes à trouver. Le niveau de difficulté par défaut est normal. Si vous augmentez le niveau de difficulté, la grille à trouver aura moins de cases noires, et si vous baissez le niveau de difficulté, elle en aura plus. Si vous cliquez une fois sur une case, elle devient noire, deux fois une croix apparaît et trois fois vous effacez la croix. En cliquant sur les niveaux de difficulté vous générerez à chaque fois une nouvelle grille à trouver correspondant au niveau de difficulté choisie. Le bouton Révéler/Cacher la solution permet, en cliquant une fois dessus, d'afficher la grille solution et en cliquant une deuxième fois dessus de cacher la solution.</b>"))
        )
      )
    )),

    tags$style(HTML("
    .my-button {
      background-color: #1E90FF;
      border: none;
      color: white;
      padding: 15px 32px;
      text-align: center;
      text-decoration: none;
      display: inline-block;
      font-size: 16px;
      margin: 4px 2px;
      cursor: pointer;
      border-radius: 8px;
    }

    .cross {
      font-size: 24px;
      color: red;
    }

    #difficultyLevel {
      font-size: 20px;
      color: #333;
      margin-top: 10px;
      font-weight: bold;
    }

    .difficulty-buttons-container {
      display: flex;
      flex-direction: column;
      align-items: flex-start;
    }
  ")),
    tags$script('
    Shiny.addCustomMessageHandler("changeColor", function(data) {
      var box = document.getElementById(data.id);
      var isDark = data.isDark;
      var clickCount = $(box).data("clickCount") || 0;

      if (clickCount === 0) {
        box.style.backgroundColor = "black";
      } else if (clickCount === 1) {
        box.style.backgroundColor = "#e0e0e0";
        var cross = document.createElement("div");
        cross.innerHTML = "✖";
        cross.className = "cross";
        // Ajuster les styles pour centrer la croix
        cross.style.display = "flex";
        cross.style.alignItems = "center";
        cross.style.justifyContent = "center";
        box.appendChild(cross);
      } else {
        box.style.backgroundColor = "#e0e0e0";
        while (box.firstChild) {
          box.removeChild(box.firstChild);
        }
      }

      $(box).data("isDark", !isDark);
      $(box).data("clickCount", (clickCount + 1) % 3);
    });

    $(document).on("click", ".clickable-box-grid1", function() {
      var boxId = $(this).attr("id");
      var isDark = $(this).data("isDark") || false;
      Shiny.setInputValue("clickedBox", {id: boxId, isDark: isDark});
    });

    Shiny.addCustomMessageHandler("fillRandomGrid", function(data) {
      $("#gridOutput2 .clickable-box").each(function() {
        $(this).css("background-color", "#e0e0e0");
        $(this).html("");
        $(this).data("isDark", false);
        $(this).data("clickCount", null);
      });
    });

    Shiny.addCustomMessageHandler("updateNumbers", function(data) {
      $("#rowNumbers").text(data.rowNumbers);
      $("#colNumbers").text(data.colNumbers);
    });

    Shiny.addCustomMessageHandler("toggleRulesVisibility", function(data) {
      var rulesContainer = $("#rulesContainer");
      var isVisible = rulesContainer.is(":visible");

      if (isVisible) {
              rulesContainer.hide();
      } else {
        rulesContainer.show();
      }
    });
  ')
  )


  # Fonction du serveur Shiny
  server <- function(input, output, session) {
    values <- reactiveValues(
      grid1 = NULL,
      grid2 = NULL,
      modifiedGrid = NULL,
      revealState = FALSE
    )

    observe({
      gridSize <- input$gridSize
      values$grid1 <- generateEmptyGrid(gridSize)
      values$grid2 <- generateRandomGrid(gridSize)
      values$modifiedGrid <- generateEmptyGrid(gridSize)
    })

    output$gridOutput1 <- renderUI({
      gridSize <- input$gridSize

      lapply(1:min(gridSize, nrow(values$grid1)), function(row) {
        div(
          div(
            id = paste0("countTextColumnGrid1_", row),
            class = "count-column",
            style = "width: 40px; height: 40px; border: 1px solid #ccc; margin: 2px; background-color: #f0f0f0; display: flex; align-items: center; justify-content: center;",
            textOutput(paste0("countTextColumnGrid1_", row))
          ),
          lapply(1:min(gridSize, ncol(values$grid1)), function(col) {
            div(
              id = paste0("grid1_", row, "_", col),
              class = "clickable-box clickable-box-grid1",
              style = sprintf(
                "width: 40px; height: 40px; background-color: %s; border: 1px solid #ccc; margin: 2px; display: flex; align-items: center; justify-content: center;",
                if (row <= nrow(values$grid1) && col <= ncol(values$grid1)) {
                  ifelse(values$grid1[row, col] == 1, "black", "#e0e0e0")
                } else {
                  "#e0e0e0"  # Valeur par défaut si l'indice est en dehors de la plage
                }
              ),
              ""
            )
          }),
          style = "display: flex; justify-content: flex-start;"
        )
      })
    })

    observe({
      gridSize <- input$gridSize

      suppressWarnings({
        lapply(1:gridSize, function(row) {
          output[[paste0("countTextColumnGrid1_", row)]] <- renderText({
            if (row <= nrow(values$grid2)) {
              row_info <- countGroups(values$grid2[row, ])
              count_cells_in_groups <- row_info$count_cells_in_groups;

              if (length(count_cells_in_groups) > 0) {
                paste(count_cells_in_groups, collapse = "-")
              } else {
                "0"
              }
            } else {
              ""
            }
          })
        })
      })
    })

    output$grid1RowNumbers <- renderUI({
      gridSize <- input$gridSize
      div(
        div(style = "width: 40px; height: 40px;"),
        lapply(1:min(gridSize, ncol(values$grid1)), function(col) {
          div(
            id = paste0("countRowGrid1_", col),
            class = "count-row",
            style = "width: 40px; height: 40px; border: 1px solid #ccc; margin: 2px; background-color: #f0f0f0; display: flex; align-items: center; justify-content: center;",
            textOutput(paste0("countTextRowGrid1_", col))
          )
        }),
        style = "display: flex; justify-content: flex-start; margin-left: 4px;"
      )
    })
    observe({
      gridSize <- input$gridSize

      tryCatch({
        lapply(1:min(gridSize, ncol(values$grid2)), function(col) {
          output[[paste0("countTextRowGrid1_", col)]] <- renderText({
            if (col <= ncol(values$grid2)) {
              col_info <- countGroups(values$grid2[, col])
              count_cells_in_groups <- col_info$count_cells_in_groups

              if (length(count_cells_in_groups) > 0) {
                paste(count_cells_in_groups, collapse = "-")
              } else {
                "0"
              }
            } else {
              ""
            }
          })
        })
      }, error = function(e) {
        print(paste("Erreur dans la boucle observe :", e$message))
      })
    })

    output$gridOutput2 <- renderUI({
      gridSize <- input$gridSize

      if (!values$revealState) {
        return(NULL)
      }

      columnSums <- colSums(values$grid2)

      div(
        div(
          id = "countColumnSum",
          class = "count-column",
          style = "display: flex; justify-content: flex-start; padding-left: 34px;",
          lapply(1:gridSize, function(col) {
            div(
              id = paste0("countColumn_", col),
              class = "count-column",
              style = "width: 40px; height: 40px; border: 1px solid #ccc; margin: 2px; background-color: #f0f0f0; display: flex; align-items: center; justify-content: center;",
              textOutput(paste0("countTextColumn_", col))
            )
          })
        ),
        div(
          lapply(1:gridSize, function(row) {
            div(
              div(
                div(
                  id = paste0("countRow_", row),
                  class = "count-column",
                  style = "width: 40px; height: 40px; border: 1px solid #ccc; margin: 2px; background-color: #f0f0f0; display: flex; align-items: center; justify-content: center;",
                  textOutput(paste0("countText_", row))
                ),
                lapply(1:gridSize, function(col) {
                  div(
                    id = paste0("grid2_", row, "_", col),
                    class = "clickable-box",
                    style = ifelse(row <= nrow(values$grid2) && col <= ncol(values$grid2),
                                   ifelse(values$grid2[row, col], "width: 40px; height: 40px; background-color: black; border: 1px solid #ccc; margin: 2px;", "width: 40px; height: 40px; background-color: #e0e0e0; border: 1px solid #ccc; margin: 2px;"),
                                   "width: 40px; height: 40px; background-color: #e0e0e0; border: 1px solid #ccc; margin: 2px;"
                    )
                  )
                }),
                style = "display: flex; justify-content: flex-start;"
              )
            )
          })
        )
      )
    })

    observe({
      gridSize <- input$gridSize
      columnSums <- colSums(values$grid2)

      output$countColumnSum <- renderText({
        paste(columnSums, collapse = " ")
      })

      lapply(    1:min(gridSize, nrow(values$grid2)), function(row) {
        count <- sum(values$grid2[row, ])
        output[[paste0("countText_", row)]] <- renderText(count)
      })


      lapply(1:min(gridSize, ncol(values$grid2)), function(col) {
        output[[paste0("countTextColumn_", col)]] <- renderText(columnSums[col])
      })

    })

    observeEvent(input$revealSolution, {
      values$revealState <- !values$revealState
    })

    observeEvent(input$clickedBox, {
      boxId <- input$clickedBox$id
      isDark <- input$clickedBox$isDark
      row_col <- strsplit(strsplit(boxId, "_")[[1]][2], "_")[[1]]
      row <- as.numeric(row_col[1])
      col <- as.numeric(row_col[2])

      values$modifiedGrid[row, col] <- as.numeric(!isDark)
      session$sendCustomMessage("changeColor", list(id = boxId, isDark = isDark))
    })

    #variable réactive pour le niveau de difficulté
    currentDifficulty <- reactiveVal("Normal")

    observeEvent(input$gridSize, {
      gridSize <- input$gridSize
      values$grid1 <- generateEmptyGrid(gridSize)
      values$grid2 <- generateRandomGrid(gridSize)
      values$modifiedGrid <- generateEmptyGrid(gridSize)
      values$revealState <- FALSE
      currentDifficulty("Normal")
    })

    observeEvent(input$hardMode, {
      gridSize <- input$gridSize
      values$grid2 <- matrix(sample(c(1,0,0), as.numeric(gridSize)^2, replace = TRUE), nrow = as.numeric(gridSize), ncol = as.numeric(gridSize))
      values$grid1 <- generateEmptyGrid(gridSize)  # Réinitialiser la grille 1
      values$modifiedGrid <- generateEmptyGrid(gridSize)  # Réinitialiser la grille modifiée
      values$revealState <- FALSE
      currentDifficulty("Difficile")

      # Mettez à jour le rendu de la grille 1
      output$gridOutput1 <- renderUI({
        gridSize <- input$gridSize

        lapply(1:gridSize, function(row) {
          div(
            div(
              id = paste0("countTextColumnGrid1_", row),
              class = "count-column",
              style = "width: 40px; height: 40px; border: 1px solid #ccc; margin: 2px; background-color: #f0f0f0; display: flex; align-items: center; justify-content: center;",
              textOutput(paste0("countTextColumnGrid1_", row))
            ),
            lapply(1:gridSize, function(col) {
              div(
                id = paste0("grid1_", row, "_", col),
                class = "clickable-box clickable-box-grid1",
                style = sprintf(
                  "width: 40px; height: 40px; background-color: %s; border: 1px solid #ccc; margin: 2px; display: flex; align-items: center; justify-content: center;",
                  if (row <= nrow(values$grid1) && col <= ncol(values$grid1)) {
                    ifelse(values$grid1[row, col] == 1, "black", "#e0e0e0")
                  } else {
                    "#e0e0e0"  # Valeur par défaut si l'indice est en dehors de la plage
                  }
                ),
                ""
              )
            }),
            style = "display: flex; justify-content: flex-start;"
          )
        })
      })
    })

    observeEvent(input$easyMode, {
      gridSize <- input$gridSize
      values$grid2 <- matrix(sample(c(1,1,1,0), as.numeric(gridSize)^2, replace = TRUE), nrow = as.numeric(gridSize), ncol = as.numeric(gridSize))
      values$grid1 <- generateEmptyGrid(gridSize)  # Réinitialiser la grille 1
      values$modifiedGrid <- generateEmptyGrid(gridSize)  # Réinitialiser la grille modifiée
      values$revealState <- FALSE
      currentDifficulty("Facile")

      # Mettez à jour le rendu de la grille 1
      output$gridOutput1 <- renderUI({
        gridSize <- input$gridSize

        lapply(1:gridSize, function(row) {
          div(
            div(
              id = paste0("countTextColumnGrid1_", row),
              class = "count-column",
              style = "width: 40px; height: 40px; border: 1px solid #ccc; margin: 2px; background-color: #f0f0f0; display: flex; align-items: center; justify-content: center;",
              textOutput(paste0("countTextColumnGrid1_", row))
            ),
            lapply(1:gridSize, function(col) {
              div(
                id = paste0("grid1_", row, "_", col),
                class = "clickable-box clickable-box-grid1",
                style = sprintf(
                  "width: 40px; height: 40px; background-color: %s; border: 1px solid #ccc; margin: 2px; display: flex; align-items: center; justify-content: center;",
                  if (row <= nrow(values$grid1) && col <= ncol(values$grid1)) {
                    ifelse(values$grid1[row, col] == 1, "black", "#e0e0e0")
                  } else {
                    "#e0e0e0"  # Valeur par défaut si l'indice est en dehors de la plage
                  }
                ),
                ""
              )
            }),
            style = "display: flex; justify-content: flex-start;"
          )
        })
      })
    })


    observeEvent(input$NormalMode, {
      gridSize <- input$gridSize
      values$grid2 <- generateRandomGrid(gridSize)
      values$grid1 <- generateEmptyGrid(gridSize)
      values$modifiedGrid <- generateEmptyGrid(gridSize)
      values$revealState <- FALSE
      currentDifficulty("Normal")

      # Mettez à jour le rendu de la grille 1
      output$gridOutput1 <- renderUI({
        gridSize <- input$gridSize

        lapply(1:gridSize, function(row) {
          div(
            div(
              id = paste0("countTextColumnGrid1_", row),
              class = "count-column",
              style = "width: 40px; height: 40px; border: 1px solid #ccc; margin: 2px; background-color: #f0f0f0; display: flex; align-items: center; justify-content: center;",
              textOutput(paste0("countTextColumnGrid1_", row))
            ),
            lapply(1:gridSize, function(col) {
              div(
                id = paste0("grid1_", row, "_", col),
                class = "clickable-box clickable-box-grid1",
                style = sprintf(
                  "width: 40px; height: 40px; background-color: %s; border: 1px solid #ccc; margin: 2px; display: flex; align-items: center; justify-content: center;",
                  if (row <= nrow(values$grid1) && col <= ncol(values$grid1)) {
                    ifelse(values$grid1[row, col] == 1, "black", "#e0e0e0")
                  } else {
                    "#e0e0e0"  # Valeur par défaut si l'indice est en dehors de la plage
                  }
                ),
                ""
              )
            }),
            style = "display: flex; justify-content: flex-start;"
          )
        })
      })
    })


    # Ajoutez une sortie pour afficher le niveau de difficulté
    output$difficultyLevel <- renderText({
      paste("Niveau : ", currentDifficulty())
    })

    observeEvent(input$toggleRules, {
      session$sendCustomMessage("toggleRulesVisibility", list())
    })
  }
  shinyApp(ui = ui, server = server)


}

