box::use(
  testthat[expect_equal, expect_length, test_that],
)

# fmt: skip
box::use(
  app/logic/visualization/color_palettes[
    create_blue_palette,
    create_intervention_palette
  ],
)

test_that("create_blue_palette works", {
  # The blues palette has 9 colors by default.
  expect_length(create_blue_palette(3), 3)
  # Test a zero as input
  expect_length(create_blue_palette(0), 0)
  expect_length(create_blue_palette(9), 9)
  # Test a larger than colors available by default number
  expect_length(create_blue_palette(20), 20)
})

test_that("create_intervention_palette works", {
  # Test with interventions including "No intervention"
  interventions <- c("Vaccine", "Bednets", "No intervention")
  pal <- create_intervention_palette(interventions)
  expect_length(pal(interventions), 3)
  expect_equal(pal("No intervention"), "grey85")

  # Test without "No intervention"
  interventions2 <- c("Vaccine", "Bednets")
  pal2 <- create_intervention_palette(interventions2)
  expect_length(pal2(interventions2), 2)

  # Test custom palette
  pal3 <- create_intervention_palette(
    interventions,
    no_intervention_color = "red",
    palette_name = "magma"
  )
  expect_equal(pal3("No intervention"), "red")
})
