import pandas as pd
import matplotlib.pyplot as plt

pd.set_option("display.float_format", "{:.4f}".format)

projecoes_tratadas = (
    pd.read_excel("~Github/saude_bucal/10_sisdim/projecoes_tratadas.xlsx")
    .assign(
        id_faixa_etaria=lambda df: df["faixa_etaria"].map({
            "0 a 14 anos": 1,
            "15 a 29 anos": 2,
            "30 a 59 anos": 3,
            "60 anos ou mais": 4
        })
    )
)

df.plot =  projecoes_tratadas[
    projecoes_tratadas["uf"].isin(["GO", "DF", "MT", "MS"])
]

plt.figure(figsize=(10, 6))
for key, grp in df_plot.groupby(['uf', 'faixa_etaria']):
    plt.plot(grp['ano'], grp['total'], label=f"{key[0]} - {key[1]}")

plt.legend()
plt.title("População por UF e Faixa Etária (Centro-Oeste)")
plt.xlabel("Ano")
plt.ylabel("Total")
plt.show()

cobertura = pd.read_csv("~Github/saude_bucal/10_sisdim/merge_sb_cobertura.csv")

cobertura_sb = (
    cobertura
    .drop(columns=["Unnamed: 0", "total_populacao", "faixa_etaria",
                                                                  "erro_padrao",
                                                                  "IC_inf", "IC_sup"])
    .assign(
        perc_necessidade=lamda df:
            df["perc_necessidade"] / 100,
            cod_procedimento=lambda df:
            df["tipo_procedimento"].map({
                "Endondontia": 1,
                "Periodontia": 2,
                "APS": 3,
                "Prótese": 4,
            })
    )
)    
