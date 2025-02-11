# Aplicativo Shiny para Análise de Gap entre Necessidade e Oferta

Este é um aplicativo Shiny que analisa a lacuna entre necessidade e oferta de serviços odontológicos no Brasil, permitindo visualizar dados por região de saúde.

## Funcionalidades

- Análise separada para APS (Atenção Primária à Saúde) e AES (Atenção Especializada)
- Mapas interativos por região de saúde
- Gráficos de barras por UF
- Tabelas detalhadas com métricas
- Parâmetros ajustáveis como tempo de procedimentos, produtividade, etc.

## Como Rodar Localmente

### Pré-requisitos

- Docker instalado
- Docker Compose instalado
- Git instalado

### Passos

1. Clone o repositório: git clone https://github.com/seu-usuario/seu-repositorio.git

2. Navegue até o diretório do projeto: cd 07_shiny_app

3. Execute o Docker Compose: docker-compose up --build

4. Acesse o aplicativo: Abra seu navegador e acesse http://localhost:3838

## Estrutura do Projeto

O aplicativo é composto por:

- `app.R`: Código principal do Shiny
- `data/`: Diretório com dados necessários
- `Dockerfile`: Configuração do container
- `docker-compose.yml`: Configuração do ambiente

## Parâmetros Ajustáveis

### APS
- Tempo médio de procedimentos
- Taxa de produtividade
- Percentual de perda

### AES
- Tempo médio por especialidade
- Taxa de produtividade
- Percentual de perda

## Visualizações

### Mapas
- Distribuição geográfica do gap por região de saúde
- Escala de cores indicando intensidade

### Gráficos
- Barras horizontais por UF
- Agrupamento por região do país

### Tabelas
- Dados detalhados por região de saúde
- Métricas de oferta e necessidade
